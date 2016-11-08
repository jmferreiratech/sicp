;;; Exercise 1.9
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))
;(+ 4 5)
;(inc (+ (dec 4) 5))
;(inc (+ 3 5))
;(inc (inc (+ (dec 3) 5)))
;(inc (inc (+ 2 5)))
;(inc (inc (inc (+ (dec 2) 5))))
;(inc (inc (inc (+ 1 5))))
;(inc (inc (inc (inc (+ (dec 1) 5)))))
;(inc (inc (inc (inc (+ 0 5)))))
;(inc (inc (inc (inc 5))))
;(inc (inc (inc 6)))
;(inc (inc 7))
;(inc 8)
;9
; This is a recursive process

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))
;(+ 4 5)
;(+ (dec 4) (inc 5))
;(+ 3 6)
;(+ (dec 3) (inc 6))
;(+ 2 7)
;(+ (dec 2) (inc 7))
;(+ 1 8)
;(+ (dec 1) (inc 8))
;(+ 0 9)
;9
; This is an iterative process

;;; Exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))
(A 1 10)
;Value: 1024
(A 2 4)
;Value: 65536
(A 3 3)
;Value: 65536
(define (f n) (A 0 n))
; (f n) computes 2n
(define (g n) (A 1 n))
; (g n) returns 2^n for n > 0 and 0 otherwise
(define (h n) (A 2 n))
; (h n) evaluates to 2^(h(n-1)) for n > 1, to 2 for n = 1 and to 0 for n = 0

;;; Exercise 1.11
; Recursive
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
	 (* 2 (f (- n 2)))
	 (* 3 (f (- n 3))))))
; Iterative
(define (f-iter i_3 i_2 i_1 count)
  (if (= 0 count)
      i_1
      (f-iter i_2
	      i_1
	      (+ i_1
		 (* 2 i_2)
		 (* 3 i_3))
	      (- count 1))))
(define (f n)
  (if (< n 3)
      n
      (f-iter 0 1 2 (- n 2))))

;;; Exercise 1.15
; a. p is applied 5 times.
; b. the order of growth is logarithmic.
