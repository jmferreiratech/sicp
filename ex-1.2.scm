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

;;; Exercise 1.16
(define (even? n)
  (= (remainder n 2) 0))
(define (fast-expt b n)
  (define (expi b n a)
    (cond ((= n 0) a)
	  ((even? n) (expi (square b) (/ n 2) a))
	  (else (expi b (- n 1) (* a b)))))
  (expi b n 1))

;;; Exercise 1.17
(define (double a)
  (+ a a))
(define (halve a)
  (/ a 2))
(define (* a b)
  (cond ((= b 0) 0)
	((even? b) (* (double a) (halve b)))
	(else (+ a (* a (- b 1))))))

;;; Exercise 1.18
(define (* a b)
  (define (*-iter a b c)
    (cond ((= b 0) c)
	  ((even?  b) (*-iter (double a) (halve b) c))
	  (else (*-iter a (- b 1) (+ c a)))))
  (*-iter a b 0))

;;; Exercise 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   (+ (square q) (square p))
		   (* q (+ q (* 2 p)))
		   (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))

;;; Exercise 1.20
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
;; Normal-order evaluation
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
2
; While expanding b was evaluated each time by the if clause causing 'remainder' to be performed 14 times. The evaluation of a causes 'remainder' to be performed 4 times. Total 18 times.
;; Applicative-order evaluation
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd 40 6)
(gcd 6 (remainder 40 6))
(gcd 6 4)
(gcd 4 (remainder 6 4))
(gcd 4 2)
(gcd 2 (remainder 4 2))
(gcd 2 0)
2
; 'remainder' is performed 4 times.

;;; Exercise 1.21
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(smallest-divisor 199)
;Value: 199

(smallest-divisor 1999)
;Value: 1999

(smallest-divisor 19999)
;Value: 7

;;; Exercise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
(define (prime? n)
  (= n (smallest-divisor n)))
(define (search-for-primes from to)
  (cond ((< from to)
	 (if (even? from)
	     (timed-prime-test (+ from 1))
	     (timed-prime-test from))
	 (search-for-primes (+ from 2) to))))

(search-for-primes 1000000000 1000000021) ; takes about 0.04 seconds
(search-for-primes 10000000000 10000000061) ; takes about 0.13 seconds
(search-for-primes 100000000000 100000000057) ; takes about 0.42 seconds
