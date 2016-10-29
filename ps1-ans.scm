;;; Evaluating expressions

(define square
  (lambda (x) (* x x)))

;;; Exploring the system
;;; Exercise 1: More debugging
;;; The procedures are fine.

;;; Exercise 2: Still more debugging
(define fact
  (lambda (n)
    (if (= n 0)
	1
	(* n (fact (- n 1))))))
(fact 243)
;Value: 57651072073405564859932599378988824389544612769748785289578514753791226660795447787952561780489668440613028916503471522241703645767996810695135226278296742637606115134300787052991319431412379312540230792060250137088708811794424564833107085173464718985508999858791970609491066045711874321516918150905413944789377156315207186998055591451670633898714567745386826936678840548225648089961727875705444538167142818292862812160000000000000000000000000000000000000000000000000000000000

;;; Exercise 3: Defining a simple procedure
(define (comb n k)
  (/ (fact n)
     (* (fact k)
	(fact (- n k)))))

(comb 243 90)
;Value: 193404342391239489855973693417880600543891038618846567058277413638164

;;; Exercise 11:
(define foo1
  (lambda (x)
    (* x x)))
(foo1 (sqrt 3))

(define foo2
  (lambda (x y)
    (/ x y)))
(foo2 6 2)

(define foo3
  (lambda (x)
    (lambda (y)
      (/ x y))))
((foo3 6) 2)

(define foo4
  (lambda (x)
    (x 3)))
(foo4 (lambda (x) x))

(define foo5
  (lambda (x)
    (cond ((= x 2)
	   (lambda () x))
	  (else
	   (lambda () (* x 3))))))
((foo5 1))

(define foo6
  (lambda (x)
    (x (lambda (y) (y y)))))
(foo6 (lambda (x) ((x (lambda (k) k)) 3)))
