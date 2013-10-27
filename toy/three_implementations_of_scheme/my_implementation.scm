;;  
;;    My Implementation
;; 注意
;; 虚拟机的compiler没有implement tail call optimization
;; tail call optimization 将会在以后 bootstrap的时候implement
;; 到时候用 toy language 写compiler生成字节码再用虚拟机运行
;; 现在的虚拟机只是core， implement最基本的内容


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
;;    argument        ; push argument on accumulator to toppest frame of stack
;;    call            ; get procedure from accumulator, pop toppest frame in stack. run function
;;    test jmp_steps  ; get value from accumulator and test, if pass, run next
;;              ; else jump
;;    jmp steps       ; jmp steps
;;	  goto pc         ; goto pc; this instruction may replace jmp in the future
;;
;;
;;
;;
;;
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


;; define toy date type
;; support:
;; ===
;; number [integer float]
;; list
;; vector
;; atom
;; boolean

(define NUMBER 1)
(define LIST 2)
(define VECTOR 3)
(define ATOM 4)
(define BOOLEAN 5)
(define INTEGER 6)
(define FLOAT 7)


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
(define (stack-pop stack) (stack 'pop))                     ;; stack pop value
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
      (frame-lookup-iter (cdr frame) var-name (+ count 1)))))
;; lookup variable in symbol table
(define (lookup table var-name)
  (define length-of-table (length table))
  (define (table-lookup-iter table var-name count)
    (if (null? table)
      (cons -1 -1) ;; cannot find var
      ;; (error "cannot find var" var-name)
      (let ((index (frame-lookup-iter (car table) var-name 0)))
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
    (if (eq? n -1) ;; var does not exist
    	(begin  ;; create space for that var, and set its value to 'undefined
    		(set-car! env (append (car env) (list var-name))) ;; extend env
    		(instructions-push instructions (make-inst 'refer (- (length env) 1) (- (length (car env)) 1) ))
    		)
    	(instructions-push instructions (make-inst 'refer n m)) ;; var exists
    	))))

(define (definition-variable exp)
  (cadr exp))
(define (definition-value exp)
  (caddr exp))
;; compile define
;; (define x 12)  define number, list, vector primitive types
;; (define x (lambda (a) a)) define lambda
;;                           when define lambda, if x does not exist, then set x at first before compiling lambda
;;                           in order to do tail call optimization(by yiyi)
(define (compile-define var value env instructions)
  ;; get var-name-index
  (let ((var-name-index (frame-lookup-iter (car env) var 0)))
     (cond ((eq? var-name-index -1)
              ;; add var-name to env
              ;; this code here has problem
              ;; (set-car! env (cons var (car env)))
              (set-car! env (append (car env) (list var))) ;; i changed code to this here
              ;; compile value
              (compile value env instructions) ;; compile value after find var-name
              ;; add 'assign
              (instructions-push instructions (make-inst 'assign (- (length env) 1) (- (length (car env)) 1))))
           (else
              ;; compile value
              (compile value env instructions)
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
        (if (eq? n -1) ;; if eq -1, then it means var does not exist
        	(error "cannot find var" var)  ;; var does not exist
        	(instructions-push instructions (make-inst 'assign n m)) ;; var exist
        	))))


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
        (vector-set! (instructions-ref instructions index1) 1 (+ length1 2))
        ;; set steps in jmp which is length2
        (vector-set! (instructions-ref instructions index2) 1 (+ length2 1))))))



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

;; this function return the num of arguments
(define (compile-args args env instructions count) ;; compile arguments ;; count is index to save
  (cond ((not (null? args)) ;; not null, so compile argument
          (compile (car args) env instructions) ;; compile arg
          (instructions-push instructions (make-inst 'argument 0 0)) ;; add argument
          (compile-args (cdr args) env instructions (+ count 1)))
        (else 
          count)))


;; compile application
(define (compile-application applic args env instructions)
  ;; add new frame
  (instructions-push instructions (make-inst 'frame 0 0 ))
  ;; compile arguments
  (compile-args args env instructions 0)
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

;; compile list
;; without calculation
(define (compile-list exp env instructions)
	(instructions-push instructions (make-inst 'frame 0 0)) ;; make argument frame
	(define (compile-list-iter exp env instructions)
		(cond ((null? exp)       ;; done
				(instructions-push instructions (make-inst 'refer 0 9)) ;; call list procedure
				(instructions-push instructions (make-inst 'call 0 0)))
			  ((pair? (car exp)) ;; list 
			  	(compile-list (car exp) env instructions)        ;; compile that list
			  	(instructions-push instructions (make-inst 'argument 0 0))          ;; push as argument
			  	(compile-list-iter (cdr exp) env instructions))  ;; continue recur
			  (else 
			  	(instructions-push instructions (make-inst 'constant (car exp) 0))  ;; read constant
			  	(instructions-push instructions (make-inst 'argument 0 0))          ;; push as argument
			  	(compile-list-iter (cdr exp) env instructions)  ;; continue recur
			  	)))
	(compile-list-iter exp env instructions))

;; compile exp
(define (compile exp env instructions)
  (cond ((symbol? exp)
   (compile-lookup exp env instructions)) ;; if symbol doesn't exist(free variable), then create new var for that symbol whos value is 'undefined
  ((pair? exp)
    (let ((tag (car exp)))
      (cond ((eq? tag 'quote)
      		 (cond ((pair? (cadr exp))
						(compile-list (cadr exp) env instructions)
						) ;; it is list, call list function to build list
      		 	   (else 
      		 	   		(instructions-push instructions (make-inst 'constant (cadr exp) 0))
      		 	   	)))
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
;; ('closure closure-body closure-env)
(define (make-closure start-pc environment) ;; start-pc is where instruction starts
  (list 'closure start-pc environment))
(define (closure-start-pc closure)          ;; get closure start point
  (cadr closure))
(define (closure-environment closure)       ;; get closure environment
  (caddr closure))
(define (closure? v) ;; check whether v is closure
	(if (and (pair? v) (eq? (car v) 'closure))
		#t
		#f))
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
          (newline)
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
                    (stack-push stack (make-stack 64)) ;; argument frame
                    (VM instructions environment acc (+ pc 1) stack))
                  ((eq? arg0 'argument) ;; add value from accumulator to frame
                  	;; push argument to argument frame according to index
					          (stack-push (stack-top stack) acc)
                    (VM instructions environment acc (+ pc 1) stack))
                  ((eq? arg0 'call)  ;; call function, pop frame that stored in stack
                  	;; consider different situations
                  	;; 1. user defined procedure(lambda)
                  	;; 2. builtin procedures (like car cdr)
                  	;; 3. vectors
                  	;; 4. dictionary
                  	(cond  	((closure? acc) ;; closure
                  				(let ((a (VM instructions                   ;; run closure
                  						 	(closure-environment-extend
                  						 		(closure-environment acc)
                  						 		(stack-top stack))
                  							 '()
                  						 	(closure-start-pc acc) ;; jmp to that pc
                  						 	stack)))
                  						(stack-pop stack) ;; pop argument frame
                  						(VM instructions (stack-pop stack) a (+ pc 1) stack) ;; restore environment from stack
                  					))
                  			((builtin-procedure? acc) ;; builtin procedure
                  				(let ((a ((get-builtin-procedure acc) (stack-pop stack))))     ;; run builtin procedure
                  					(VM instructions (stack-pop stack) a (+ pc 1) stack) ;; restore environment from stack
                  					))
                  			((vector? acc)  ;; vector
                  				(let ((a (apply-vector-procedure (stack-pop)))) ;; call vector related function
                  					(VM instructions (stack-pop stack) a (+ pc 1) stack) ;; restore environment from stack
                  					))
                  			(else
                  				(error "Invalid calling"))
                  		))
                  ((eq? arg0 'goto)
                    (VM instructions environment acc arg1 stack))
                  ((eq? arg0 'test) ;; test jmp
                    (if acc ;; if pass acc run next; else jmp
                      (VM instructions environment acc (+ pc 1) stack)
                      (VM instructions environment acc (+ pc arg1) stack)))
                  ((eq? arg0 'jmp)  ;; jmp forward or back
                    (VM instructions environment acc (+ pc arg1) stack))
                  ((eq? arg0 'close) ;; make closure
                    ;; arg1 is new pc
                    (VM instructions environment (make-closure (+ pc 1) environment) (+ 1 arg1) stack))
                  ((eq? arg0 'return) ;; end closure return value in accumulator
                    acc)
                  (else (error "Invalid instruction" arg0 arg1 arg2))))))))


;; apply-vector-procedure
;; ([1 2] 0) => 1
;; ([1 2 3] 0 12) => [12 2 3]
(define (apply-vector-procedure vec param-stack)
	(let ((len (stack-length param-stack)))
		(cond ((eq? len 1)
				(vector-ref vec (stack-ref param-stack 0)))
			  ((eq? len 2)
			  	(vector-set! vec (stack-ref param-stack 0) (stack-ref param-stack 1)))
			  (else
			  	(error "Vector -- invalid parameters")))))

;; builtin primitive procedures
;; return (builtin-procedure procedure)
;;  car cdr cons eq? display
;;  + - * / 
;;
(define (builtin-procedure? v) ;; check whether value is builtin procedure
	(and (pair? v) (eq? (car v) 'builtin-procedure)))
(define (get-builtin-procedure v) ;; get proc content of builtin procedure
	(cadr v))
(define (make-builtin-procedure proc) ;; make builtin procedure
	(list 'builtin-procedure proc))

;; this function may be used in the future
(define (satisfy-params param-stack param-num)  ;; check whether meet param num
	(let ((len (stack-length param-stack)))
		(cond ((eq? param-num len)
				#t)
			  ((> len param-num)
			  	(error "Too many parameters provided")
			  	#f)
			  (else
			  	(error "Too few parameters provided")
			  	#f))))

;; 1 car
(define _car (lambda (param-stack)
	(let ((arg (stack-ref param-stack 0)))
		(car arg))))
(define builtin-car (make-builtin-procedure _car)) ;; make car function

;; 2 cdr
(define _cdr (lambda (param-stack)
	(let ((arg (stack-ref param-stack 0)))
		(cdr arg))))
(define builtin-cdr (make-builtin-procedure _cdr)) ;; make cdr function

;; 3 cons
(define _cons (lambda (param-stack)
	(let ((arg0 (stack-ref param-stack 0))
		  (arg1 (stack-ref param-stack 1)))
		(cons arg0 arg1))))
(define builtin-cons (make-builtin-procedure _cons)) ;; make cons function

;; 4 eq?
(define _eq? (lambda (param-stack)
	(let ((arg0 (stack-ref param-stack 0))
		  (arg1 (stack-ref param-stack 1)))
		(eq? arg0 arg1))))
(define builtin-eq? (make-builtin-procedure _eq?)) ;; make eq? function

;; 5 display
(define _display (lambda (param-stack)
	(let ((arg (stack-ref param-stack 0)))
		(display arg))))
(define builtin-display (make-builtin-procedure _display)) ;; make display function

;; 6 +
(define _+ (lambda (param-stack)
	(let ((arg0 (stack-ref param-stack 0))
		  (arg1 (stack-ref param-stack 1)))
		(+ arg0 arg1))))
(define builtin-+ (make-builtin-procedure _+))

;; 7 -
(define _- (lambda (param-stack)
	(let ((arg0 (stack-ref param-stack 0))
		  (arg1 (stack-ref param-stack 1)))
		(+ arg0 arg1))))
(define builtin-- (make-builtin-procedure _-))


;; 8 *
(define _* (lambda (param-stack)
	(let ((arg0 (stack-ref param-stack 0))
		  (arg1 (stack-ref param-stack 1)))
		(+ arg0 arg1))))
(define builtin-* (make-builtin-procedure _*))


;; 9 /
(define _/ (lambda (param-stack)
	(let ((arg0 (stack-ref param-stack 0))
		  (arg1 (stack-ref param-stack 1)))
		(+ arg0 arg1))))
(define builtin-/ (make-builtin-procedure _/))

;; 10 list
;; build list
(define _list (lambda (param-stack)
	(define (list-iter output param-stack count)
		(if (eq? count -1)
			output
			(list-iter (cons (stack-ref param-stack count) output) param-stack (- count 1))))
	(list-iter '() param-stack (- (stack-length param-stack) 1))))
(define builtin-list (make-builtin-procedure _list))

;; 11 vector
;; build vector
(define _vector (lambda (param-stack) param-stack))
(define builtin-vector (make-builtin-procedure _vector))

(define builtin-procedure-name-list      ;; builtin-procedure-name-list, save builtin name and put to symbol table
	(list 'car 'cdr 'cons 'eq? 'display '+ '- '* '/  
		  'list 'vector
	  ))
(define builtin-procedure-list
	(list builtin-car builtin-cdr builtin-cons builtin-eq? builtin-display
		builtin-+ builtin-- builtin-* builtin-/
		builtin-list builtin-vector
		)) 

;;
;;
;;
;;
;;
;;
;;
;; make global frame
;; which holds primitive-builtin-procedures
(define (make-global-frame)
  (define (add-builtin-procedures stack builtin-procedure-list)
  	(if (null? builtin-procedure-list)
  		stack
  		(begin
  			(stack-push stack (car builtin-procedure-list)) ;; push function to stack
  			(add-builtin-procedures stack (cdr builtin-procedure-list)) ;; recur
  			)))	
  (let ((s (make-stack 256))) ;; init stack as frame
  	;; add primitive builtin procedures
  	(add-builtin-procedures s builtin-procedure-list)
    ))


;; =======================================
;; make environment for virtual machine
;; that environment is stack
(define (make-environment)
  (let ((env-array (make-stack 64)) ;; can store 64 frames.. like global, local1, local2...
        (global-frame (make-global-frame))  ;; global frame
       )
    (stack-push env-array global-frame) ;; push global-frame to env-array
    env-array))
;; define environment operation functions
(define (environment-set! environment n m set-value)    ;; set value at specific frame
  (stack-set! (stack-ref environment n) m set-value))         
(define (environment-ref environment n m)               ;; ref value at specfic position n m of env-array
  (stack-ref (stack-ref environment n) m))
(define (environment-display environment)
  (stack-display environment))


;;==============================================================================
(define (make-symbol-table) ;; make symbol table for compile
	(list builtin-procedure-name-list))

;;==================================== TEST ====================================
;; create empty environment
;; (define (make-empty-env-for-compiler) '(()))
(define env (make-symbol-table))

(display "Finish Initializing Env")
(newline)

(define instructions (make-instructions))

(display "Finish Initializing Instructions")
(newline)

;; (define x '((define x (lambda (a) a)) x) )
(define x '(
	(if #f 2 (+ 3 4))
		)
)
(compile-sequence x env instructions)
(instructions-display instructions)
(newline)



;; run 
(define my-env (make-environment))
(VM instructions my-env '() 0 (make-stack 1024)) ;; test virtual machine





















