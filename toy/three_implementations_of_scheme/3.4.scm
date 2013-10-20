;; heap based scheme
;; compiler
;; (lambda (a b) (+ a b))
(define (lambda-parameters x)
  (car (cdr x)))
(define (lambda-body x)
  (cdr (cdr x)))
;; if a 1 2
(define (if-test x)
  (car (cdr x)))
(define (if-consequent x)
  (car (cdr (cdr x))))
(define (if-alternative x)
  (car (cdr (cdr (cdr x)))))
; set! x 12
(define (assignment-variable x)
  (car (cdr x)))
(define (assignment-value x)
  (car (cdr (cdr x))))
(define (tail? next) 
  (eq? (car next) 'return))

(define loop (lambda (args c next)
  (if (null? args)
    (if (tail? next)
      c
      (list 'frame next c))
    (loop (cdr args)
          (compile (car args) 
                   (list 'argument c))
          next))))

(define (compile x next)
  (cond ((symbol? x)
   (list 'refer x next))
  ((pair? x)
   (cond ((eq? (car x) 'quote)
    (list 'constant obj next))
   ((eq? (car x) 'lambda)
    (list 'close 
      (lambda-parameters x)
      (compile (lambda-body x) '(return))
      next))
   ((eq? (car x) 'if)
    (let ((test (if-test x))
      (consequent (if-consequent x))
      (alternative (if-alternative x)))
    (compile test (list 'test consequent alternative))))
   ((eq? (car x) 'set!)
    (compile (assignement-value x)
     (list 'assign 
       (assignment-variable x)
       next)))
   (else  
                  (loop (cdr x) ;; args
                    (compile (car x) '(apply))
                    next)
                  )))
  (else (list 'constant x next))))

;; a accumulator
;; x expression
;; e environment
;; r rib
;; s stack
(define (VM a x e r s)
  (cond ((eq? (car x) 'halt)
   a)
  ((eq? (car x) 'refer)
   (VM (car (lookup (car (cdr x)) e))
     x e r s))
  ((eq? (car x) 'constant)
   (VM (car (cdr x))
     x e r s))
  ((eq? (car x) 'close)
   (VM (closure (closure-body x)
    (closure-env x)
    (closure-args x))
   x e r s))
  ((eq? (car x) 'test)
   (VM a
     (if a
       (test-consequent x)
       (test-alternative x))
     e r s))
  ((eq? (car x) 'assign)
   (set-car! (lookup (car (cdr x))
     e)
   a)
   (VM a x e r s))
        ;; frame
  ((eq? (car x) 'frame)
    (let ((ret (car (cdr x)))
          (exp (car (cdr (cdr x))))))
    (VM a exp e '() (call-frame ret e r s)))

  ((eq? (car x) 'argument) ;; argument
    (VM a x e (cons a r) s))
  
  ))


(define x '(if 1 12 13))
(display (compile x '(halt)))














