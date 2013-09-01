;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;                       Chapter_6.scm
;;                       originated from TLS
;;                       edited by Lawrence R. Amlord(颜世敏 Shi-min Yan)
;;                       informlarry@gmail.com
;;                       Sept 1st, 2013
;;                       Xi'an, China

;; Copyright (C) 1974-2013 Daniel P. Friedman and Matthias Felleisen
;; <informlarry@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define atom?
  (lambda (x)
    (and (not (null? x)) (not (pair? x)))))
;Value: atom?

(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((eq? (car (cdr aexp)) '+)
      (and (numbered? (car aexp))
	   (numbered? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) '*)
      (and (numbered? (car aexp))
	   (numbered? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) '^)
      (and (numbered? (car aexp))
	   (numbered? (car (cdr (cdr aexp)))))))))
;Value: numbered?

(numbered? 1)
;Value: #t

(numbered? '(3 + (4 ^ 5)))
;Value: #t

(numbered? '(2 * sausage))
;Value: #f

(define numbered2?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else
      (and (numbered2? (car aexp))
	   (numbered2? (car (cdr (cdr aexp)))))))))
;Value: numbered2?

(numbered2? 1)
;Value: #t

(numbered2? '(3 + (4 ^ 5)))
;Value: #t

(numbered2? '(2 * sausage))
;Value: #f

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car (cdr nexp)) '+)
      (+ (value (car nexp))
	 (value (car (cdr (cdr nexp))))))
     ((eq? (car (cdr nexp)) '*)
      (* (value (car nexp))
	 (value (car (cdr (cdr nexp))))))
     (else
      (expt (value (car nexp))
	    (value (car (cdr (cdr nexp)))))))))
;Value: value

(value 13)
;Value: 13

(value '(1 + 3))
;Value: 4

(value '(1 + (3 ^ 4)))
;Value: 82

(value 'cookie)
;Value: cookie



(define value2
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car nexp) '+)
      (+ (value2 (cdr nexp))
	 (value2 (cdr (cdr nexp)))))
     ((eq? (car nexp) '*)
      (* (value2 (cdr nexp))
	 (value2 (cdr (cdr nexp)))))
     (else
      (expt (value2 (cdr nexp))
	    (value2 (cdr (cdr nexp))))))))
;Value: value2

(value2 '(+ 1 3))

;The object (), passed as the first argument to cdr, is not the correct type.
;To continue, call RESTART with an option number:
; (RESTART 2) => Specify an argument to use in its place.
; (RESTART 1) => Return to read-eval-print level 1.

(RESTART 1)

;Abort!



(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))
;Value: 1st-sub-exp

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))
;Value: 2nd-sub-exp

(define operator
  (lambda (aexp)
    (car aexp)))
;Value: operator

(define value3
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) '+)
      (+ (value3 (1st-sub-exp nexp))
	 (value3 (2nd-sub-exp nexp))))
     ((eq? (operator nexp) '*)
      (* (value3 (1st-sub-exp nexp))
	 (value3 (2nd-sub-exp nexp))))
     (else
      (expt (value3 (1st-sub-exp nexp))
	    (value3 (2nd-sub-exp nexp)))))))
;Value: value3

(value3 '(+ 1 3))
;Value: 4



(define 1st-sub-exp2
  (lambda (aexp)
    (car aexp)))
;Value: 1st-sub-exp2

(define 2nd-sub-exp2
  (lambda (aexp)
    (car (cdr (cdr aexp)))))
;Value: 2nd-sub-exp2

(define operator2
  (lambda (aexp)
    (car (cdr aexp))))
;Value: operator2


(define value4
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator2 nexp) '+)
      (+ (value4 (1st-sub-exp2 nexp))
	 (value4 (2nd-sub-exp2 nexp))))
     ((eq? (operator2 nexp) '*)
      (* (value4 (1st-sub-exp2 nexp))
	 (value4 (2nd-sub-exp2 nexp))))
     (else
      (expt (value4 (1st-sub-exp2 nexp))
	    (value4 (2nd-sub-exp2 nexp)))))))
;Value: value4

(value4 '(1 + 3))
;Value: 4



(define sero?
  (lambda (n)
    (null? n)))
;Value: sero?

(define edd1
  (lambda (n)
    (cons '() n)))
;Value: edd1

(define zub1
  (lambda (n)
    (cdr n)))
;Value: zub1

(zub1 '())

;The object (), passed as the first argument to cdr, is not the correct type.
;To continue, call RESTART with an option number:
; (RESTART 2) => Specify an argument to use in its place.
; (RESTART 1) => Return to read-eval-print level 1.

(RESTART 1)

;Abort!

(define o+
  (lambda (n m)
    (cond
     ((sero? m) n)
     (else (edd1 (o+ n (zub1 m)))))))
;Value: o+

(edd1 '())
;Value 18: (())

(zub1 (edd1 '()))
;Value: ()

(sero? '())
;Value: #t

(sero? (zub1 (edd1 '())))
;Value: #t

(o+ '(()) '(() ()))
;Value 19: (() () ())

(define o-
  (lambda (n m)
    (cond
     ((sero? m) n)
     (else (zub1 (o- n (zub1 m)))))))
;Value: o-

(o- '(() () ()) '(()))
;Value 20: (() ())

(define o*
  (lambda (n m)
    (cond
     ((sero? m) '())
     (else (o+ n (o* n (zub1 m)))))))
;Value: o*

(o* '(() () ()) '(() ()))
;Value 21: (() () () () () ())

(define o<
  (lambda (n m)
    (cond
     ((sero? m) #f)
     ((sero? n) #t)
     (else (o< (zub1 n) (zub1 m))))))
;Value: o<

(define o/
  (lambda (n m)
    (cond
     ((o< n m) '())
     (else (edd1 (o/ (o- n m) m))))))
;Value: o/

(o/ '(() () () () () () () () () ())
    '(() ()))
;Value 22: (() () () () ())

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))
;Value: lat?

(lat? '(1 2 3))
;Value: #t

(lat? '((()) (() ()) (() () ())))
;Value: #f

