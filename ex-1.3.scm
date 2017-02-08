;;; Exercise 1.29
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (cube x)
  (* x x x))
(integral cube 0 1 0.001)
;Value: .249999875000001

(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (coef k)
    (cond ((= 0 k) 1)
	  ((= n k) 1)
	  ((even? k) 2)
	  (else 4)))
  (define (term k)
    (* (coef k) (f (+ a (* k h)))))
  (define (inc x) (+ 1 x))
  (* (/ h 3.0)
     (sum term 0 inc n)))

(integral-simpson cube 0 1 100)
;Value: .25

(integral-simpson cube 0 1 1000)
;Value: .25

;;; Exercise 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0))

;;; Exercise 1.31a
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (factorial n)
  (define (id x) x)
  (define (inc x) (+ 1 x))
  (product id 1 inc n))

(define (pi n)
  (define (num-term k)
    (if (even? k)
	(num-term (+ k 1))
	(+ k 1)))
  (define (den-term k)
    (if (even? k)
	(den-term (- k 1))
	(+ k 2)))
  (define (inc k) (+ 1 k))
  (* 4.0
     (/ (product num-term 1.0 inc n)
	(product den-term 1.0 inc n))))

;;; Exercise 1.31b
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))
  (iter a 1))

;;; Exercise 1.32a
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (define (add a b) (+ a b))
  (accumulate add 0 term a next b))
(define (product term a next b)
  (define (times a b) (* a b))
  (accumulate times 1 term a next b))

;;; Exercise 1.32a
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner result (term a)))))
  (iter a null-value))

;;; Exercise 1.33
(define (accumulate combiner null-value term a next b valid?)
  (cond ((> a b) null-value)
        ((valid? a) (combiner (term a) (accumulate combiner null-value term (next a) next b)))
        (else (accumulate combiner null-value term (next a) next b))))

;;; Exercise 1.33a
(define (sum-squared-primes a b)
  (define (add-square x y) (+ (square x) (square y)))
  (define (id x) x)
  (define (inc x) (+ 1 x))
  (accumulate add-square 0 id a inc b prime?))

;;; Exercise 1.33b
(define (weird-sum n)
  (define (id x) x)
  (define (inc x) (+ 1 x))
  (define (filter i) (= 1 (gcd i n)))
  (accumulate * 1 id 1 inc (- n 1) filter))

;;; Exercise 1.35
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
	       1.0))

(golden-ratio)
;Value: 1.6180327868852458

;;; Exercise 1.36
(define (verbose-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display next)
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(verbose-fixed-point (lambda (x) (/ (log 1000) (log x)))
		     2.0)
;Value: 4.555532270803653 (34 steps)
(define (average a b)
  (/ (+ a b) 2))
(verbose-fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
		     2.0)
;Value: 4.555537551999825 (9 steps)
