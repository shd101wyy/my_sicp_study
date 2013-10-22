;;  
;;    My Implementation


;;    For the Compiler Machine
;;    exp environment 

;;    For the virtual Machine
;;    accumulator
;;    stack

;;    instructions
;;    const value     ; save value in accumulator
;;    refer n m       ; get value from stack n, m and save to accumulator
;;    assign n m      ; get value from accumulator and save it to stack n m
;;    close index-of-return ; create closure
;;    return          ; end closure
;;    frame           ; create new frame on stack
;;    argument        ; add argument on accumulator to stack most on top frame
;;    call            ; get procedure from accumulator, pop toppest frame in stack. run function
;;    test jmp_steps  ; get value from accumulator and test, if pass, run next
;;              ; else jump
;;    jmp steps       ; jmp steps
;;    

;; closure .  procedure_body procedure_arguments procedure_env
;;

;; (define (test a) a)
;; close
;;     refer 1 0
;; assign 0 0

;; toy language
;; ⟨core⟩ → ⟨object⟩
;; ⟨core⟩ → ⟨variable⟩
;; ⟨core⟩→(quote ⟨object⟩)
;; ⟨core⟩→(lambda (⟨variable⟩ ...) ⟨core⟩) 
;; ⟨core⟩ → (if ⟨core⟩ ⟨core⟩ ⟨core⟩)
;; ⟨core⟩ → (set! ⟨variable⟩ ⟨core⟩) 
;; ⟨core⟩→(call/cc ⟨core⟩)
;;  ⟨core⟩→(⟨core⟩ ⟨core⟩ ...)
(define (cadr x) (car (cdr x)) )
(define (caddr x) (car (cdr (cdr x)))) 
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cddr x) (cdr (cdr x)))
(define (cdddr x) (cdr (cdr (cdr x))))

;; =========== Done Compiler =============
;; =========== Now Virtual Machine Part ==
;; make stack
(define (make-stack size) ;; make stack according to size
  (let ((length 0) ;; length of stack
        (stack (make-vector size)))   ;; stack
    (lambda (msg)
      (cond ((eq? msg 'length) 
               length) ;; return length
              ((eq? msg 'stack) ;; show stack
               stack)
              ((eq? msg 'ref) 
                (lambda (i) (vector-ref stack i)))
              ((eq? msg 'push) ;; push value
               (lambda (push-val)
                  (cond ((eq? length stack)                   ;; check overflow
                          (error "Stack Overflow"))
                        (else 
                          (vector-set! stack length push-val) ;; push value
                          (set! length (+ length 1))          ;; update length
                          ))))        
              ((eq? msg 'pop)    ;; pop stack
               (set! length (- length 1))
               (vector-ref stack length))
              ((eq? msg 'stack-set!)                          ;; stack set value at index
                (lambda (i set-value) 
                  (vector-set! stack i set-value)))
              ((eq? msg 'top)    ;; return top element of stack
                (vector-ref stack (- length 1)))))))

(define (stack-display stack) (display (stack 'stack))) ;; display stack
(define (stack-push stack push_val) ((stack 'push) push_val)) ;; stack push value
(define (stack-pop stack) ((stack 'pop)))                     ;; stack pop value
(define (stack-length stack) (stack 'length))                 ;; return stack length
(define (stack-ref stack index) ((stack 'ref) index))
(define (stack-top stack) (stack 'top))
(define (stack-set! stack index set-value) ((stack 'stack-set!) index set-value)) ;; stack set value at index
;; =====================


;; define instructions
;; all single instruction must have 2 arguments
(define (make-inst arg0 arg1 arg2)
  (define v (make-vector 3))
  (vector-set! v 0 arg0)
  (vector-set! v 1 arg1)
  (vector-set! v 2 arg2)
  v)
(define (make-instructions)
  (define size 50)               ;; default size 1000
  (define length 0)                ;; i don't know how to increase size
  (define v (make-vector 50))
  (define (instructions-push inst) ;; push inst to instructions
    (cond ((eq? length size)       ;; overflow
           (error "Instructions overflow... Idk how to increase size" length size))
          (else (vector-set! v length inst)    ;; push
                (set! length (+ length 1)))))  ;; increase length
  (define (dispatch msg)
    (cond ((eq? msg 'length)       ;; return length
            length)
          ((eq? msg 'push)         ;; push value
            (lambda (push_value) (instructions-push push_value)))
          ((eq? msg 'display)
            (display v))
          ((eq? msg 'ref)
            (lambda (i) (vector-ref v i)))))
  dispatch)
(define (instructions-push insts push-inst) ;; push
  ((insts 'push) push-inst))
(define (instructions-length insts)         ;; get length
  (insts 'length))
(define (instructions-display insts)        ;; display instructions
  (insts 'display)) 
(define (instructions-ref insts i)          ;; get inst from insts
  ((insts 'ref) i))

;; ========================


;;
;; symbol table
;;  local   global
;; ((a b c) (x y z))
(define (add-frame table frame)
  (cons frame table))
(define (extend-symbol-table table vars)
  (cons vars table))
;; check var in list
;; eg 'a '(a b c) => 0
;;    'c '(a b) => -1
(define (in? var-name list)
  (define (in?-iter var-name list count)
    (cond ((null? list) -1) ;; didn't find
          ((eq? var-name (car list)) count) ;; find return count
          (else (in?-iter var-name (cdr list) (+ count 1)))))
  (in?-iter var-name list 0))

;; return index of var at frame
(define (frame-lookup-iter frame var-name count)
  (if (null? frame)
    -1 ;; didn't find
    (if (eq? var-name (car frame))
      count 
      (frame-lookup-iter (cdr frame) var-name (- count 1)))))
;; lookup variable in symbol table
(define (lookup table var-name)
  (define length-of-table (length table))
  (define (table-lookup-iter table var-name count)
    (if (null? table)
      (error "cannot find var" var-name)
      (let ((index (frame-lookup-iter (car table) var-name (- (length (car table)) 1))))
        (if (eq? index -1) 
          (table-lookup-iter (cdr table) var-name (- count 1))
          (cons count index)
          ))))
  (table-lookup-iter table var-name (- length-of-table 1)))



;; env stack:
;;  global   local
;; [[a,b,c],[d,e,f]]
(define (compile-lookup var-name env instructions)
  (let ((n_m (lookup env var-name)))
    (let ((n (car n_m))
      (m (cdr n_m)))
    (instructions-push instructions (make-inst 'refer n m)))))

(define (definition-variable exp)
  (cadr exp))
(define (definition-value exp)
  (caddr exp))
;; compile define
(define (compile-define var value env instructions)
  ;; compile value
  (compile value env instructions)
  ;; get var-name-index
  (let ((var-name-index (frame-lookup-iter (car env) var (- (length (car env)) 1))))
     (cond ((eq? var-name-index -1)
            ;; add var-name to env
            (set-car! env (cons var (car env)))
            ;; add 'assign
            (instructions-push instructions (make-inst 'assign (- (length env) 1) (- (length (car env)) 1))))
           (else
            ;; var-name existed
            (instructions-push instructions (make-inst 'assign (- (length env) 1) var-name-index))
            ))))

;; set! x 12
(define (assignment-variable exp)
  (cadr exp))
(define (assignment-value exp)
  (caddr exp))
;; compile set!
(define (compile-set! var value env instructions)
  ;; compile value
  (compile value env instructions)
  ;; assign
  (let ((n_m (lookup env var)))
    (let ((n (car n_m))
          (m (cdr n_m)))
      (instructions-push instructions (make-inst 'assign n m)))))


(define (if-test exp)
  (cadr exp))
(define (if-consequent exp)
  (caddr exp))
(define (if-alternative exp)
  (cadddr exp))

;; compile and return the length of increased insts
(define (compile-and-return-length exp env instructions)
  (let ((old-length (instructions-length instructions)))
    ;; compile exp
    (compile exp env instructions)
    ;; get new length - old-length
    (- (instructions-length instructions) old-length)))
;; if 1 2 3
;; const 1
;; test 1
;; const 2
;; jmp 1
;; const 3
;; halt
(define (compile-if test consequent alternative env instructions)
  ;; compile test
  (compile test env instructions)
  (let ((index1 (instructions-length instructions))) ;; add test and save index
    (instructions-push instructions (make-inst 'test 0 0))
    (let ((length1 (compile-and-return-length consequent env instructions)) ;; compile consequent and return length
          (index2 (instructions-length instructions))) ;; add jmp and save index
      (instructions-push instructions (make-inst 'jmp 0 0))
      (let ((length2 (compile-and-return-length alternative env instructions))) ;; compile alternative and return length
         ;; set jmp in test which is the length1
        (vector-set! (instructions-ref instructions index1) 1 length1)
        ;; set steps in jmp which is length2
        (vector-set! (instructions-ref instructions index2) 1 length2)))))



;; compile lambda(procedure)
(define (lambda-arguments exp)
  (car (cdr exp)))
(define (lambda-body exp)
  (cdr (cdr exp)))
(define (compile-lambda args body env instructions)
  ;; save current index, which is the index of (close arg1)
  (let ((index (instructions-length instructions)))
    ;; add close
    (instructions-push instructions (make-inst 'close 0 0))
    ;; compile body
    (compile-sequence body (extend-symbol-table env args) instructions)
    ;; add return
    (instructions-push instructions (make-inst 'return 0 0))
    ;; set (close arg1).  arg1 to index of return
    (vector-set! (instructions-ref instructions index) 1 (- (instructions-length instructions) 1))))


;; compile application
;; (add 3 4)
(define (application-head exp)
  (car exp))
(define (application-args exp)
  (cdr exp))

;; compile args
;; (add 3 4) => (3 4)
;; const 3 
;; argument
;; const 4 
;; argument
(define (compile-args args env instructions) ;; compile arguments
  (cond ((not (null? args)) ;; not null, so compile argument
          (compile (car args) env instructions) ;; compile arg
          (instructions-push instructions (make-inst 'argument 0 0)) ;; add argument
          (compile-args (cdr args) env instructions))))

;; compile application
(define (compile-application applic args env instructions)
  ;; add new frame
  (instructions-push instructions (make-inst 'frame 0 0 ))
  ;; compile arguments
  (compile-args args env instructions)
  ;; compile applic
  (compile applic env instructions)
  ;; call function
  (instructions-push instructions (make-inst 'call 0 0)))


;; compile sequence
;; ((define x 12) (set! x 15))
(define (compile-sequence seq env instructions)
  (define (compile-sequence-iter seq env instructions)
    (if (null? seq)
      (begin
        ;; (instructions-push instructions (make-inst 'halt 0 0))
        instructions)
      (begin
          (compile (car seq) env instructions)
          (compile-sequence-iter (cdr seq) env instructions)
        )))
  (compile-sequence-iter seq env instructions))

;; compile exp
(define (compile exp env instructions)
  (cond ((symbol? exp)
   (compile-lookup exp env instructions))
  ((pair? exp)
    (let ((tag (car exp)))
      (cond ((eq? tag 'quote)
              (instructions-push instructions (make-inst 'constant (cadr exp) 0)))
            ((eq? tag 'define)
              (compile-define (definition-variable exp)
                              (definition-value exp)
                              env
                              instructions))
            ((eq? tag 'set!)
              (compile-set! (assignment-variable exp)
                (assignment-value exp)
                env 
                instructions))
            ((eq? tag 'if)
              (compile-if (if-test exp)
                (if-consequent exp)
                (if-alternative exp)
                env
                instructions))
            ((eq? tag 'lambda)
              (compile-lambda (lambda-arguments exp)
                (lambda-body exp)
                env
                instructions))
            (else (compile-application  (application-head exp)
              (application-args exp)
              env
              instructions)))))
  ;; constants
  (else (instructions-push instructions (make-inst 'constant exp 0)))))


;; return closure value
;; (closure-body closure-env)
(define (make-closure start-pc environment) ;; start-pc is where instruction starts
  (cons start-pc environment))
(define (closure-start-pc closure)          ;; get closure start point
  (car closure))
(define (closure-environment closure)       ;; get closure environment
  (cdr closure))
(define (closure-environment-extend base-env extend-env) ;; extend closure environment
  (stack-push base-env extend-env)
  base-env)

;; Virtual Machine that run commands
;; instructions: Instructions that need to be run
;; environment: used to save value
;; acc: every calculating result is saved there
;; pc: count
;; stack: used to save frame
(define (VM instructions environment acc pc stack)
  (cond ((eq? pc (instructions-length instructions)) ;; finish running program
          ;; return value stored in acc
          (display "End...")
          (display acc)
          acc)
        (else (let ((inst (instructions-ref instructions pc)))
          (let ((arg0 (vector-ref inst 0)) ;; get arg0
                (arg1 (vector-ref inst 1)) ;; get arg1
                (arg2 (vector-ref inst 2))) ;; get arg2
            (newline)       ;; debug use
            (display inst)  ;;
            (display arg0)  ;;
            (display arg1)  ;;
            (display arg2)  ;;

            (cond ((eq? arg0 'constant) ;; constant
                   (VM instructions environment arg1 (+ pc 1) stack)) ;; save constant in accumulator
                  ((eq? arg0 'refer)    ;; get value from environment
                    (VM instructions environment (environment-ref environment arg1 arg2) (+ pc 1) stack)) ;; save value in accumulator
                  ((eq? arg0 'assign)
                    ;; set value to environment 
                    (environment-set! environment arg1 arg2 acc)
                    (VM instructions environment acc (+ pc 1) stack)) ;; set value from accumulator to environment
                  ((eq? arg0 'frame) ;; add new frame with size 256
                    (stack-push stack environment) ;; save current env
                    (VM instructions environment acc (+ pc 1) (stack-push stack (make-stack 64))))
                  ((eq? arg0 'argument) ;; add value from accumulator to frame
                    (VM instructions environment acc (+ pc 1) (stack-push (stack-top) accumulator)))
                  ((eq? arg0 'call)  ;; call function, pop frame that stored in stack
                    (let ((a  (VM instructions   ;; run closure
                                  (closure-environment-extend 
                                    (closure-environment acc)
                                    (stack-pop stack))
                                  '()
                                  (closure-start-pc acc) ;; jmp to that pc
                                  stack)))
                        (VM instructions (stack-pop stack) a (+ pc 1) stack))) ;; restore environment from stack
                  ((eq? arg0 'test) ;; test jmp
                    (if acc ;; if pass acc run next; else jmp
                      (VM instructions environment acc (+ pc 1) stack)
                      (VM instructions environment acc (+ pc 1 arg1) stack)))
                  ((eq? arg0 'jmp)  ;; jmp forward or back
                    (VM instructions environment acc (+ pc 1 arg1) stack))
                  ((eq? arg0 'close) ;; make closure
                    ;; arg1 is new pc
                    (VM instructions environment (make-closure (+ pc 1) environment) (+ 1 arg1) stack))
                  ((eq? arg0 'return) ;; end closure return value in accumulator
                    acc)
                  (else (error "Invalid instruction" arg0 arg1 arg2))))))))


;; create empty environment
;; (define (make-empty-env-for-compiler) '(()))
(define env '((x y)))

(display "Finish Initializing Env")
(newline)

(define instructions (make-instructions))

(display "Finish Initializing Instructions")
(newline)

(define x '((define x 12) x) )
(compile-sequence x env instructions)
(instructions-display instructions)
(newline)
;; (stack-display env)

;; make environment for virtual machine
(define (make-environment)
  (let ((env-array (make-stack 64)) ;; can store 64 frames.. like global, local1, local2...
        (global-frame (make-stack 256))  ;; global frame
       )
    (define (dispatch msg)
      (cond ((eq? msg 'ref)    ;; environment-array ref
             (lambda (n m) (stack-ref (stack-ref env-array n) m)))
            ((eq? msg 'set!)   ;; set value at specific frame
              (lambda (n m set-value)
                (stack-set! (stack-ref env-array n) m set-value)))
            ((eq? msg 'display)
              (stack-display env-array))
        ))
    (stack-push env-array global-frame) ;; push global-frame to env-array
    dispatch))
(define (environment-set! environment n m set-value)    ;; set value at specific frame
  ((environment 'set!) n m set-value))       
(define (environment-ref environment n m)               ;; ref value at specfic position n m of env-array
  ((environment 'ref) n m))
(define (environment-display environment)
  (environment 'display))

(define my-env (make-environment))
(VM instructions my-env '() 0 (make-stack 1024)) ;; test virtual machine





















