;;; Exercise 1.1
10
;Value: 10

(+ 5 3 4)
;Value: 12

(- 9 1)
;Value: 8

(/ 6 2)
;Value: 3

(+ (* 2 4) (- 4 6))
;Value: 6

(define a 3)
;Value: a

(define b (+ a 1))
;Value: b

(= a b)
;Value: #f

(if (and (> b a) (< b (* a b)))
    b
    a)
;Value: 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;Value: 16

(+ 2 (if (> b a) b a))
;Value: 6

(* (cond ((> a b) a)
	 ((< a b) b)
	 (else -1))
   (+ a 1))
;Value: 16

;;; Exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;;; Exercise 1.3
(define (square x)
  (* x x))
(define (sum-of-squares a b)
  (+ (square a) (square b)))
(define (sum-of-squares-of-largers a b c)
  (if (not (< a b))
      (sum-of-squares a (if (> b c) b c))
      (sum-of-squares b (if (> a c) a c))))

;;; Exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; It sums b to a when b is positive and subtract b from a otherwise.

;;; Exercise 1.5
; An interpreter that uses applicative-order evaluation will freeze up.
; An interpreter that uses normal-order evaluation will return 0.
; The interpreter using applicative-order will try to evaluate the expression
; (p) which will lead to an infinite loop (as it evaluates to itself) while the
; interpreter using normal order won't try to evaluate it and will return the
; consequent (which is 0).
