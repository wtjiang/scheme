;;;Test cases for Scheme.
;;;
;;;In order to run only a prefix of these examples, add the line
;;;
;;;(exit)
;;;
;;;after the last test you wish to run.

20
;expect 20

(+ 200 250)
;expect 450

(- 950 450)
;expect 500

(* 10 200)
;expect 2000

(/ 30 5)
;expect 6

(+ 3.1 10)
;expect 13.1

(+ 22 10 5 17)
;expect 54

(* 10 12 4)
;expect 480

(+ (* 4 8) (- 9 3))
;expect 38

(+ (* 10 (+ (* 3 8) (+ 4 2))) (+ (- 9 2) 3))
;expect 310

(+ (* 2
      (+ (* 3 8)
         (+ 4 9)))
   (+ (- 9 2)
      5))
;expect 86

(define ten (* 5 2))
;expect ten

ten
;expect 10

(define (factorial x)
  (if (< x 2)
    1
    (* x (factorial (- x 1)))))
;expect factorial

(factorial 10)
;expect 3628800

(define (g x) (* x 5))
;expect g

(factorial (g ten))
;expect 30414093201713378043612608166064768844377641568960512000000000000

(define (deep-map fn s)
  (cond ((null? s) '())
  ((list? (car s))
    (cons (deep-map fn (car s))
      (deep-map fn (cdr s))))
      (else (cons (fn (car s))
    (deep-map fn (cdr s))))))
;expect deep-map

(define (square x) (* x x))
;expect square

(define (triple x) (* (* x x) x))
;expect triple

(deep-map square '(1 2 (3 4  (5  6)  ((7))  8)  (9  10)))
;expect (1 4 (9 16 (25 36) ((49)) 64) (81 100))

(deep-map triple '(1 2 (3 4  (5  6)  ((7))  8)  (9  10)))
;expect (1 8 (27 64 (125 216) ((343)) 512) (729 1000))

(define x 20)
;expect x

(define y (* 3 x))
;expect y

(+ y (* y 5) 10)
;expect 370

;QUOTE FORM
'(1 (two 3 . (4 . 5)))
;expect (1 (two 3 4 . 5))

(eval (cons 'car '('(1 2))))
;expect 1

'(1 2 3 4)
;expect (1 2 3 4)

;BEGIN FORM

(begin (print 3) '(+ 2 3))
;prints 3  and returns (+ 2 3)

(define x (begin (display 3) (newline) (+ 2 3)))
;displays 3 and x

(+ x 5)
;expect 10

(define x 0)
;expect x

(begin (define x 5)
       (+ x 1))
;expect 6

;LET FORM 
(let ((x 42)) x 1 2)
;expect 2

(let ((a 5) (b 9))
  (let ((c (* a b)))
    c))
  ;expect 45

(let ((a (lambda (x y) (+ x y))) (b 5) (c 7))
  (a b c))
;expect 12

(let ((a (lambda (x y) (* x y))) (b 5) (c 7))
  (a b c))
;expect 35

(let ((x 2) (y 3))
  (let ((x 7)
        (z (+ x y)))
    (* z x)))   
;expect 35

(let ((x 2) (y 3))
  (let ((x 7)
         (z (* x y)))
    (+ z x)))  
;expect 13

(let ((x '(1 3 5 7 9)))
  (+ (car x) (car(cdr x)))
  )
;expect 4

;LAMBDA FORM
((lambda (a b) (+ (* 2 a) b)) 5 6)
;expect 16

(define fact (lambda (n) 
  (if (= n 0)
    1
    (* n (fact (- n 1))))))
;expect fact

(fact 4)
;expect 24 

(define gcd
    (lambda (a b)
        (if (= a b)
          a
          (if (> a b) (gcd (- a b) b) (gcd a (- b a))))))
;expect gcd

(gcd 120 10)
;expect 10

;MU FORM

(define f (mu (b) (+ a b)))
;expect f

(define g (lambda (a b) (f (+ a b))))
;expect g

(g 5 6)
;expect 16

(define f (mu (b) (* a b)))
;expect f

(define g (lambda (a b) (f (- a b))))
;expect g

(g 2 9)
;expect -14

(define f (mu (b) (/ a b)))
;expect f

(define g (lambda (a b) (f (+ a b))))
;expect g

(g 0 1)
;expect 0

(define f (mu (b) (< a b)))
;expect f

(define g (lambda (a b) (f (+ a b))))
;expect g

(g 4 6)
;expect True


;DEFINE FORM

(define three 3)
;expect three

(define four 4)
;expect four

(+ three four)
;expect 7

(define (substitute s old new)
  (cond
   ((null? s) nil)
   ((list? (car s)) (cons (substitute (car s) old new) (substitute (cdr s) old new)))
   ((eq? (car s) old) (cons new (substitute (cdr s) old new)))
   (else (cons (car s) (substitute (cdr s) old new))))
)
;expect substitute 

(substitute '((test1) (test2) (test3) test4) 'test5 'test6)
;expect ((test1) (test2) (test3) test4)

(define (substitute-list s olds news)
  (cond
   ((null? (cdr olds)) (substitute s (car olds) (car news)))
   (else
    (substitute-list (substitute s (car olds) (car news)) (cdr olds) (cdr news))))
  )
;expect substitute-list 

(substitute-list
                  '((1 one) (2 two) (3 three) (4 four))
                  '(1 2 3 4)
                  '(one two three four))
;expect ((one one) (two two) (three three) (four four))

;IF FORM
(if (= 4 4) (* 1 2) (+ 3 4))
;expect 2

(if nil 1 0)
;expect 1

(if 0 1 2)
;expect 1

(if (1/0) 0 1)
;expect Error

;OR FORM
 (or (= 2 2) (> 2 1))
 ;expect True

 (or #f #f #f)       
 ;expect False

 (or #f #t #f)
 ;expect True

 (or #t #t #t)
 ;expect True

 (or)
 ;expect False

;AND FORM
(and)
;expect True

(and (= 2 2) (= 6 (+ 3 2) #t))
;expect Error

(and #t #f 42 (/ 1 0))
;expect False

(and 4 5 (* 6 2))
;expect 12

;COND FORM
(cond ((= 2 9) 'nope)
           ((= 10 10) 'this)
           (else 'else))
;expect this

(cond ((> 3 3) 'greater)
                ((< 3 3) 'less)
                (else 'equal))
;expect equal

(cond ((not nil) 'nil_is_false)
      ((not 0) 'zero_is_false)
      ((not (null? nil)) 'nil_is_not_null)
      (else 'all_false)
 )
;expect all_false

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scheme Implementations ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;len outputs the length of list s
(define (len s)
  (if (eq? s '())
    0
    (+ 10 (len (cdr s)))))
(len '(2 39 10 4))
;expect 40

;;;;;;;;;;;;;;;;;;;;
;;; Extra credit ;;;
;;;;;;;;;;;;;;;;;;;;


;Tail call optimization test
(define (sum n total)
  (if (zero? n) total
    (sum (- n 1) (+ n total))))

(sum 300 20)
;expect 45170

(exit)

;;; These are examples from several sections of "The Structure
;;; and Interpretation of Computer Programs" by Abelson and Sussman.

;;; License: Creative Commons share alike with attribution

;;; 1.1.1

10
;expect 10

(+ 137 349)
;expect 486

(- 1000 334)
;expect 666

(* 5 99)
;expect 495

(/ 10 5)
;expect 2

(+ 2.7 10)
;expect 12.7

(+ 21 35 12 7)
;expect 75

(* 25 4 12)
;expect 1200

(+ (* 3 5) (- 10 6))
;expect 19

(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))
;expect 57

(+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))
;expect 57


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Move the following (exit) line to run additional tests. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;1.1.2

(define size 2)
;expect size
size
;expect 2

(* 5 size)
;expect 10

(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))
;expect 314.159

(define circumference (* 2 pi radius))
circumference
;expect 62.8318

;;;1.1.4

(define (square x) (* x x))
;expect square
(square 21)
;expect 441

(define square (lambda (x) (* x x))) ;See Section 1.3.2
(square 21)
;expect 441

(square (+ 2 5))
;expect 49

(square (square 3))
;expect 81

(define (sum-of-squares x y)
  (+ (square x) (square y)))
(sum-of-squares 3 4)
;expect 25

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
(f 5)
;expect 136

;;;1.1.6

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
(abs -3)
;expect 3

(abs 0)
;expect 0

(abs 3)
;expect 3

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
(a-plus-abs-b 3 -2)
;expect 5

;;;1.1.7

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))
(sqrt 9)
;expect 3.00009155413138

(sqrt (+ 100 37))
;expect 11.704699917758145

(sqrt (+ (sqrt 2) (sqrt 3)))
;expect 1.7739279023207892

(square (sqrt 1000))
;expect 1000.000369924366

;;;1.1.8

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
(sqrt 9)
;expect 3.00009155413138

(sqrt (+ 100 37))
;expect 11.704699917758145

(sqrt (+ (sqrt 2) (sqrt 3)))
;expect 1.7739279023207892

(square (sqrt 1000))
;expect 1000.000369924366

;;;1.3.1

(define (cube x) (* x x x))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))
(sum-cubes 1 10)
;expect 3025

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))
(sum-integers 1 10)
;expect 55

;;;1.3.2

((lambda (x y z) (+ x y (square z))) 1 2 3)
;expect 12

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
(f 3 4)
;expect 456

(define x 5)
(+ (let ((x 3))
     (+ x (* x 10)))
   x)
;expect 38

(let ((x 3)
      (y (+ x 2)))
  (* x y))
;expect 21

;;;2.1.1

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define x (cons 1 2))
(car x)
;expect 1

(cdr x)
;expect 2

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))
(car (car z))
;expect 1

(car (cdr z))
;expect 3

z
;expect ((1 . 2) 3 . 4)

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (display (numer x))
  (display '/)
  (display (denom x))
  (newline))
(define one-half (make-rat 1 2))
(print-rat one-half)
;expect 1/2

(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))
;expect 5/6

(print-rat (mul-rat one-half one-third))
;expect 1/6

(print-rat (add-rat one-third one-third))
;expect 6/9 

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(print-rat (add-rat one-third one-third))
;expect 2/3

(define one-through-four (list 1 2 3 4))
one-through-four
;expect (1 2 3 4)

(car one-through-four)
;expect 1

(cdr one-through-four)
;expect (2 3 4)

(car (cdr one-through-four))
;expect 2

(cons 10 one-through-four)
;expect (10 1 2 3 4)

(cons 5 one-through-four)
;expect (5 1 2 3 4)

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(map abs (list -10 2.5 -11.6 17))
;expect (10 2.5 11.6 17)

(map (lambda (x) (* x x))
     (list 1 2 3 4))
;expect (1 4 9 16)

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))
(scale-list (list 1 2 3 4 5) 10)
;expect (10 20 30 40 50)

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
(define x (cons (list 1 2) (list 3 4)))
(count-leaves x)
;expect 4

(count-leaves (list x x))
;expect 8

;;;2.2.3

(define (odd? x) (= 1 (remainder x 2)))
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(filter odd? (list 1 2 3 4 5))
;expect (1 3 5)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(accumulate + 0 (list 1 2 3 4 5))
;expect 15

(accumulate * 1 (list 1 2 3 4 5))
;expect 120

(accumulate cons nil (list 1 2 3 4 5))
;expect (1 2 3 4 5)

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 7)
;expect (2 3 4 5 6 7)

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(enumerate-tree (list 1 (list 2 (list 3 4)) 5))
;expect (1 2 3 4 5)

;;;2.3.1

(define a 1)

(define b 2)

(list a b)
;expect (1 2)

(list 'a 'b)
;expect (a b)

(list 'a b)
;expect (a 2)

(car '(a b c))
;expect a

(cdr '(a b c))
;expect (b c)

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))
(memq 'apple '(pear banana prune))
;expect False

(memq 'apple '(x (apple sauce) y apple pear))
;expect (apple pear)

(define (equal? x y)
  (cond ((pair? x) (and (pair? y)
                        (equal? (car x) (car y))
                        (equal? (cdr x) (cdr y))))
        ((null? x) (null? y))
        (else (eq? x y))))
(equal? '(1 2 (three)) '(1 2 (three)))
;expect True

(equal? '(1 2 (three)) '(1 2 three))
;expect False

(equal? '(1 2 three) '(1 2 (three)))
;expect False

;;;Peter Norvig tests (http://norvig.com/lispy2.html)

(define double (lambda (x) (* 2 x)))
(double 5)
;expect 10

(define compose (lambda (f g) (lambda (x) (f (g x)))))
((compose list double) 5)
;expect (10)

(define apply-twice (lambda (f) (compose f f)))
((apply-twice double) 5)
;expect 20

((apply-twice (apply-twice double)) 5)
;expect 80

(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))
(fact 3)
;expect 6

(fact 50)
;expect 30414093201713378043612608166064768844377641568960512000000000000

(define (combine f)
  (lambda (x y)
    (if (null? x) nil
      (f (list (car x) (car y))
         ((combine f) (cdr x) (cdr y))))))
(define zip (combine cons))
(zip (list 1 2 3 4) (list 5 6 7 8))
;expect ((1 5) (2 6) (3 7) (4 8))

(define riff-shuffle (lambda (deck) (begin
    (define take (lambda (n seq) (if (<= n 0) (quote ()) (cons (car seq) (take (- n 1) (cdr seq))))))
    (define drop (lambda (n seq) (if (<= n 0) seq (drop (- n 1) (cdr seq)))))
    (define mid (lambda (seq) (/ (length seq) 2)))
    ((combine append) (take (mid deck) deck) (drop (mid deck) deck)))))
(riff-shuffle (list 1 2 3 4 5 6 7 8))
;expect (1 5 2 6 3 7 4 8)

((apply-twice riff-shuffle) (list 1 2 3 4 5 6 7 8))
;expect (1 3 5 7 2 4 6 8)

(riff-shuffle (riff-shuffle (riff-shuffle (list 1 2 3 4 5 6 7 8))))
;expect (1 2 3 4 5 6 7 8)

;;;Additional tests

(apply square '(2))
;expect 4

(apply + '(1 2 3 4))
;expect 10

(apply (if false + append) '((1 2) (3 4)))
;expect (1 2 3 4)

(if 0 1 2)
;expect 1

(if '() 1 2)
;expect 1

(or false true)
;expect True

(or)
;expect False

(and)
;expect True

(or 1 2 3)
;expect 1

(and 1 2 3)
;expect 3

(and False (/ 1 0))
;expect False

(and True (/ 1 0))
;expect Error

(or 3 (/ 1 0))
;expect 3

(or False (/ 1 0))
;expect Error

(or (quote hello) (quote world))
;expect hello

(if nil 1 2)
;expect 1

(if 0 1 2)
;expect 1

(if (or false False #f) 1 2)
;expect 2

(define (loop) (loop))
(cond (false (loop))
      (12))
;expect 12

((lambda (x) (display x) (newline) x) 2)
;expect 2 ;2

(define g (mu () x))
(define (high f x)
  (f))

(high g 2)
;expect 2

(define (print-and-square x)
  (print x)
  (square x))
(print-and-square 12)
;expect 12 ;144

(/ 1 0)
;expect Error

(define addx (mu (x) (+ x y)))
(define add2xy (lambda (x y) (addx (+ x x))))
(add2xy 3 7)
;expect 13


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scheme Implementations ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;len outputs the length of list s
(define (len s)
  (if (eq? s '())
    0
    (+ 1 (len (cdr s)))))
(len '(1 2 3 4))
;expect 4

;;;;;;;;;;;;;;;;;;;;
;;; Extra credit ;;;
;;;;;;;;;;;;;;;;;;;;


;Tail call optimization test
(define (sum n total)
  (if (zero? n) total
    (sum (- n 1) (+ n total))))
(sum 1001 0)
;expect 501501


