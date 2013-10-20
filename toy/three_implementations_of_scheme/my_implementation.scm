;;  
;;    My Implementation


;;    For the Compiler Machine
;;    exp environment 

;;    For the virtual Machine
;;    accumulator
;;    stack

;;    instructions
;;    refer n m       ; get value from stack n, m and save to accumulator
;;    assign n m      ; get value from accumulator and save it to stack n m
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
               (vector-set! stack length push-val) ;; push value
               (set! length (+ length 1))))        ;; update length
            ((eq? msg 'pop)    ;; pop stack
             (set! length (- length 1))
             vector-ref stack length)))))

(define (stack-display stack) (display (stack 'stack))) ;; display stack
(define (stack-push stack push_val) ((stack 'push) push_val)) ;; stack push value
(define (stack-pop stack) ((stack 'pop)))                     ;; stack pop value
(define (stack-length stack) (stack 'length))                 ;; return stack length
(define (stack-ref stack index) ((stack 'ref) index))



;; env
;;  global    local
;; [[x y z], [a b c]]
(define (env-add-var-name env n var-name)
  (define frame (stack-ref env n))
  (if (eq? (stack-indexOf frame var-name) -1)
    (stack-push frame var-name)
    #f))


;; find var in index in stack
;; [a, b, c] find b => 1
(define (stack-indexOf stack find-value)
  (define (stack-indexOf stack find-value current-index)
    (if (eq? (stack-length stack) current-index)
      -1 ;; didn't find
      (if (eq? find-value (stack-ref stack current-index))
        current-index
        (stack-indexOf stack find-value (+ current-index 1)))))
  (stack-indexOf stack find-value 0))

;; find var from env
(define (lookup var-name env)
  (define (lookup-iter var-name env i)
    (if (= i (stack-length env))
      (error "Undefined var" var-name)
      (let ((index-of-var (stack-indexOf (stack-ref env i) var-name)))
        (if (= index-of-var -1)
          (lookup-iter var-name env (+ i 1)) ;; didn't find var in current frame
          (cons i index-of-var)))))          ;; find var
  (lookup-iter var-name env 0))



;; env stack:
;;  global   local
;; [[a,b,c],[d,e,f]]
(define (compile-lookup var-name env next)
  (let ((n_m (lookup var-name env)))
    (let ((n (car n_m))
      (m (cdr n_m)))
    (cons (list 'refer n m) next))
    ))

(define (definition-variable exp)
  (cadr exp))
(define (definition-value exp)
  (caddr exp))
;; compile define
(define (compile-define var value env next)
  (let ((compiled-value (car (compile value env '())))
    (var-name-index (stack-indexOf (stack-ref env (- (stack-length env) 1))
     var)))
    (if (eq? var-name-index -1)
        (begin 
          (env-add-var-name env (- (stack-length env) 1) var) ;; add var-name to env
          (cons compiled-value (cons (list 'assign (- (stack-length env) 1) (- (stack-length (stack-ref env (- (stack-length env) 1))) 1) ) next));; var-name-doesn't existed
          ) 
        (cons compiled-value (cons (list 'assign (- (stack-length env) 1) var-name-index) next)) ;; var-name exist
        )))

(define (assignment-variable exp)
  (cadr exp))
(define (assignment-value exp)
  (caddr exp))
(define (compile-set! var value env next)
  (let ((compiled-value (car (compile value env '())))
    (n_m (lookup var env)))
  (let ((n (car n_m))
    (m (cdr n_m)))
  (cons compiled-value (cons (list 'assign n m) next)))))

(define (if-test exp)
  (cadr exp))
(define (if-consequent exp)
  (caddr exp))
(define (if-alternative exp)
  (cadddr exp))
;; if 1 2 3
;; const 1
;; test 3
;; const 2
;; jmp 2
;; const 3
(define (compile-if test consequent alternative env next)
  (let ((compiled-test (car (compile test env '())))
    (compiled-consequent (car (compile-sequence consequent env '())))
    (compiled-alternative (car (compile-sequence alternative env '()))))
  (cons compiled-test (cons (list 'test (length compiled-consequent))
    (cons compiled-consequent
      (cons (list 'jmp (length compiled-alternative))
        (cons compiled-alternative next)))))))


;; compile lambda(procedure)
(define (lambda-arguments exp)
  (cdr exp))
(define (lambda-body exp)
  (cdr (cdr exp)))
(define (compile-lambda args body env next)
  (define new-frame (make-stack 256))
  (define (extend-args args new-frame env)
    (if (null? args)
      (stack-push env new-frame)
      (begin
        (stack-push new-frame (car args))
        (compile-lambda (cdr args) body new-frame env))))
  (cons '(close) (cons (compile-sequence body (extend-args args new-frame env) next) '(return))))


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
(define (compile-args args env)
  (if (null? args) 
    '()
    (cons (list 'argument) 
      (cons (compile (car args) env '())
        (compile-args (cdr args) env)))))


(define (compile-application applic args env next)
  (let ((compiled-args (cons '(frame) (compile-args args env)))
    (compiled-applic (compile applic env '())))
  (cons compiled-args (cons compiled-applic (cons '(apply) next)))))

;; compile sequence
;; ((define x 12) (set! x 15))
(define (compile-sequence seq env next)
  (define (compile-sequence-iter seq env next)
    (if (null? seq)
      next
      (cons (compile (car seq) env '())
        (compile-sequence-iter (cdr seq) env next))))
  (compile-sequence-iter seq env next))

(define (compile exp env next)
  (cond ((symbol? exp)
   (compile-lookup var-name env next))
  ((pair? exp)
    (let ((tag (car exp)))
      (cond ((eq? tag 'quote)
        (cons (list 'constant (cadr exp)) next))
      ((eq? tag 'define)
        (compile-define (definition-variable exp)
                        (definition-value exp)
                        env
                        next))
      ((eq? tag 'set!)
        (compile-set! (assignment-variable exp)
          (assignment-value exp)
          env 
          next))
      ((eq? tag 'if)
        (compile-if (if-test exp)
          (if-consequent exp)
          (if-alternative exp)
          env
          next))
      ((eq? tag 'lambda)
        (compile-lambda (lambda-arguments exp)
          (lambda-body exp)
          env
          next))
      (else (compile-application  (application-head exp)
        (application-args exp)
        env
        next)))))
  (else (cons (list 'constant exp) next))))

(define (display-instructions insts)
  (cond ((null? insts)
   (newline)
   (display 'Done!))
  (else
   (newline)
   (display (car insts))
   (display-instructions (cdr insts)))))

;; create empty environment
(define (make-empty-env)
  (define stack (make-stack 1000))
  (define global-frame (make-stack 256))
  (stack-push stack global-frame)
  stack)

(define env (make-empty-env))
(define x '((define x 12) (set! x 15)))
(display-instructions (compile-sequence x env '((halt)) ) )
(newline)
;; (stack-display env)









