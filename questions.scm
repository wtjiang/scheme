(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.
(define (map proc items)
  ; start
  (if (null? items) nil
   (cons (proc (car items)) (map proc (cdr items)))
  )
  ; end
)

(define (cons-all first rests)
  ; start
  (map (lambda (rest) (cons first rest)) rests)
  ; end
)


(define (zip pairs)
  ; start
  (if (null? pairs) nil
      (list (map car pairs) (map cadr pairs))
  )
  ; end
)

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (helper s counter)
    (if (null? s) nil
      (cons (cons counter (cons (car s) nil)) (helper (cdr s) (+ 1 counter)))
    )
  )
  (helper s 0)
)
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cond ((null? denoms) nil)
    ((= total 0) (list nil))
    ((< total 0) nil)
    ((< total (car denoms)) (list-change total (cdr denoms)))
    (else (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) (list-change total (cdr denoms))))
  )
)
  ; END PROBLEM 18


;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr)))
)

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr) expr)
        ((quoted? expr) expr)
        ((or (lambda? expr) (define? expr))
         (let ((form   (car expr))
              (parameters (cadr expr))
              (body   (cddr expr)))
          (cons form (cons parameters (let-to-lambda body)))
         )
        )
        ((let? expr)
          (let ((vals (cadr expr))
               (body (cddr expr)))
               (cons (cons 'lambda (cons (car(zip (let-to-lambda vals))) (let-to-lambda body))) (cadr (zip (let-to-lambda vals))))
          )
        )
        (else (map let-to-lambda expr))
  )
)
