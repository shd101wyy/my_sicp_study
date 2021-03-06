;; toy language
;; ⟨core⟩ → ⟨object⟩
;; ⟨core⟩ → ⟨variable⟩
;; ⟨core⟩→(quote ⟨object⟩)
;; ⟨core⟩→(lambda (⟨variable⟩ ...) ⟨core⟩) 
;; ⟨core⟩ → (if ⟨core⟩ ⟨core⟩ ⟨core⟩)
;; ⟨core⟩ → (set! ⟨variable⟩ ⟨core⟩) 
;; ⟨core⟩→(call/cc ⟨core⟩)
;;  ⟨core⟩→(⟨core⟩ ⟨core⟩ ...)
(define (cadr x) (car (cdr x)))
(define (caddr x) (car (cdr (cdr x)))) 
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cddr x) (cdr (cdr x)))
(define (cdddr x) (cdr (cdr (cdr x))))

;; environment
;; frame enclosing env
;; (frame enclosing env)

;; here env is symbol list
;; (a b c)         0 frame
;; ((a b) c d)     1 frame
;; lookup variable in env

;; extend env
;; add var name list to env
;; new-env: '(a b)    enclosing-env: '(c d)
;; => '((a b) c d)
(define (extend-env new-env base-env)
  (cons new-env base-env))
(define (frame-var-count env)
  (length (car env)))
(define (env-length env)
  (length env))


;; check var in list
;; eg 'a '(a b c) => 0
;;    'c '(a b) => -1
(define (in? var-name list)
  (define (in?-iter var-name list count)
    (cond ((null? list) -1) ;; didn't find
          ((eq? var-name (car list)) count) ;; find return count
          (else (in?-iter var-name (cdr list) (+ count 1)))))
  (in?-iter var-name list 0))

;; look up var-name from env
(define (compile-lookup var-name env return)
  (define (compile-lookup-iter var-name env return frame-num)
    (if (null? env)
        (error "Undefined var " var-name)
        (let ((val (in? var-name (car env))))
          (if (= val -1) ;; didn't find var
              (compile-lookup-iter var-name (cdr env) return (+ frame-num 1)) ;; didn't find var in current frame, search next frame
              (return frame-num val) ;; find var
           ))))
  (compile-lookup-iter var-name env return 0))

;; lambda 
;; (lambda (a b) (+ a b))
;; (lambda [params] [body])
(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))
(define (last-exp? body)
  (null? (cdr body)))
;; compile lambda
;; (lambda (a) 
(define (compile-lambda body extended-env next)
  (define (compile-body body env)
    (cond ((last-exp? body)
           (compile (car body) env (cons '(return) next) ))
          (else
           (compile (car body) env (compile-body (cdr body) env)))))
  (cons '(closure) (compile-body body extended-env)))


;; if
;; (if judge consequent alternative)
(define (if-judge exp)
  (cadr exp))

(define (if-consequent exp)
  (caddr exp))

(define (if-alternative exp)
  (if (null? (cdddr exp))
      'false  ;; no alternative exists, return false
      (cadddr exp)))

;; assignment 
;; (set! x 12)
(define (assignment-variable exp)
  (cadr exp))
(define (assignment-value exp)
  (caddr exp))

;; definition
;; (define x 12)
(define (definition-variable exp)
  (cadr exp))
(define (definition-value exp)
  (caddr exp))
(define (compile-definition variable value env next)
  (let ((variable-index (in? variable (car env))))
    (if (= variable-index -1)  ;; variable doesn't exist
        (compile value env 
                 (cons (list 'assign 0 (length (car env))) next))
        (compile value env
                 (cons (list 'assign 0 variable-index) next)))))

;; ============= compile procedure ==================
;; (add 3 4)
;; compile add first

;; compile parameters
;; (frame)   ;; create new frame
;; (const 3) ;; 
;; (const 4)
;; (ref 0 0) ;; get add
;; (apply)   ;; then apply

(define (compile-arguments arguments env)
  ;; (define result '(frame)
  (define (compile-arguments-iter arguments env result)
    (if (null? arguments)
        result
        (compile-arguments-iter (cdr arguments) 
                                env 
                                (cons result 
                                      (compile (car arguments) env '(argument)))
                                )))
  (compile-arguments-iter arguments env '(frame)))

;; compile procedure
(define (compile-procedure proc arguments env next)
  (define (compile-arguments-iter arguments env compiled-proc)
    (cond ((null? arguments) compiled-proc)
          (else (cons (car (compile (car arguments) env '()))
                      (cons '(arguments) 
                            (compile-arguments-iter (cdr arguments) env compiled-proc)))))) 
  (let ((compiled-proc (compile proc env '((apply)))))
    (cons '(frame) (compile-arguments-iter arguments env compiled-proc))))

;;==================


;; compile exp
(define (compile exp env next)
  (cond ((symbol? exp)    ;; symbol
         (compile-lookup exp
                         env
                         (lambda (n m)
                                   (cons (list 'ref n m) next)))) ;; look up variable from env
        ((pair? exp)      ;; pair
         (let ((tag (car exp)))  ;; get tag of exp; eg (define x 12) => define
           (cond ((eq? tag 'quote) ;; quote
                  (cons (list 'constant (cadr exp)) next ))
                 ;; ============= Lambda ===============
                 ((eq? tag 'lambda) ;; lambda calculus
                  (compile-lambda (lambda-body exp) (extend-env (lambda-parameters exp) env) next))
                 ;; ============= If ====================
                 ((eq? tag 'if)
                  (let ((consequent (if-consequent exp))
                        (alternative (if-alternative exp))
                        (judge (if-judge exp)))
                    (compile judge env (cons '(test)
                                              (compile consequent env
                                                       (compile alternative env next))))))
                 ;; ============ set! ==================
                 ((eq? tag 'set!)
                  (let ((var-name (assignment-variable exp))  ;; get variable name
                        (var-value (assignment-value exp)))   ;; get variable value
                    (compile-lookup var-name 
                                    env
                                    (lambda (n m)
                                      (compile var-value 
                                               env
                                               (cons (list 'assign 
                                                           n
                                                           m)
                                                     next))))))
                 ;; ============= definition ===========
                 ((eq? tag 'define)
                  (let ((var-name (definition-variable exp))
                        (var-value (definition-value exp)))
                    (compile-definition var-name var-value env next)
                    ))
                 ;; ====================================
                 (else (compile-procedure (car exp)
                                          (cdr exp)
                                          env
                                          next))
            )))
        (else  (cons (list 'constant exp) next)) ;; constant
   ))

;; setup symbol list
(define (setup-symbol-table) 
  '(()))
(define (display-instructions insts)
  (cond ((null? insts)
         (newline)
         (display 'Done!))
        (else
         (newline)
         (display (car insts))
         (display-instructions (cdr insts)))))

;; (define symbol-table (setup-symbol-table))
(define symbol-table '((y x)))
;; (define x '((lambda (a) a) 12)  )

(define x '((lambda (b a) (define x 12) a) 12) )
(display (compile x symbol-table '()))
(newline)
(display-instructions (compile x symbol-table '()))

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


