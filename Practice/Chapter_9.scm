;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;                       Chapter_9.scm
;;                       originated from TLS
;;                       edited by Lawrence R. Amlord(颜世敏 Shi-min Yan)
;;                       informlarry@gmail.com
;;                       Sept 3rd, 2013
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
    (and (not (null? x))
	 (not (pair? x)))))
;Value: atom?

(define add1
  (lambda (n)
    (+ n 1)))
;Value: add1

(define sub1
  (lambda (n)
    (- n 1)))
;Value: sub1

(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else
      (add1 (o+ n (sub1 m)))))))
;Value: o+

(define o*
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else
      (o+ n (o* n (sub1 m)))))))
;Value: o*

(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else
      (pick (sub1 n) (cdr lat))))))
;Value: pick

(define build
  (lambda (s1 s2)
    (cons s1
	  (cons s2 '()))))
;Value: build

(define first
  (lambda (p)
    (car p)))
;Value: first

(define second
  (lambda (p)
    (car (cdr p))))
;Value: second

(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))
;Value: a-pair?

(define revpair
  (lambda (p)
    (build (second p) (first p))))
;Value: revpair



(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))
;Value: looking

(define keep-looking
  (lambda (a sorn lat)
    (cond
     ((number? sorn)
      (keep-looking a (pick sorn lat) lat))
     (else
      (eq? sorn a)))))
;Value: keep-looking

(looking 'caviar
	 '(6 2 4 caviar 5 7 3))
;Value: #t

(looking 'caviar
	 '(6 2 grits caviar 5 7 3))
;Value: #f

(looking 'caviar
	 '(7 1 2 caviar 5 6 3))

;Quit!

(define eternity
  (lambda (x)
    (eternity x)))
;Value: eternity

(define shift
  (lambda (pair)
    (build (first (first pair))
	   (build (second (first pair))
		  (second pair)))))
;Value: shift

(shift '((a b) c))
;Value 13: (a (b c))

(shift '((a b) (c d)))
;Value 14: (a (b (c d)))



(define align
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (align (shift pora)))
     (else
      (build (first pora)
	     (align (second pora)))))))
;Value: align

(define length*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else
      (o+ (length* (first pora))
	  (length* (second pora)))))))
;Value: length*

(define weight*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else
      (o+ (o* (weight* (first pora)) 2)
	  (weight* (second pora)))))))
;Value: weight*

(weight* '((a b) c))
;Value: 7

(weight* '(a (b c)))
;Value: 5

(define shuffle
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (shuffle (revpair pora)))
     (else
      (build (first pora)
	     (shuffle (second pora)))))))
;Value: shuffle

(shuffle '(a (b c)))
;Value 15: (a (b c))

(shuffle '(a b))
;Value 16: (a b)

(shuffle '((a b) (c d)))

;Quit!



(define C
  (lambda (n)
    (cond
     ((one? n) 1)
     (else
      (cond
       ((even? n) (C (/ n 2)))
       (else
	(C (add1 (o* 3 n)))))))))
;Value: c



(define A
  (lambda (n m)
    (cond
     ((zero? n) (add1 m))
     ((zero? m) (A (sub1 n) 1))
     (else
      (A (sub1 n)
	 (A n (sub1 m)))))))
;Value: a

(A 1 0)
;Value: 2

(A 1 1)
;Value: 3

(A 2 2)
;Value: 7

(A 1 2)
;Value: 4

(A 4 3)

;Quit!



(define length
  (lambda (l)
    (cond
     ((null? l) 0)
     (else
      (add1 (length (cdr l)))))))
;Value: length

((lambda (l)
   (cond
    ((null? l) 0)
    (else
     (add1 (eternity (cdr l))))))
 '())
;Value: 0

((lambda (l)
   (cond
    ((null? l) 0)
    (else
     (add1 (eternity (cdr l))))))
 '(1))

;Quit!



((lambda (l)
   (cond
    ((null? l) 0)
    (else
     (add1
      ((lambda (l)
	(cond
	 ((null? l) 0)
	 (else
	  (add1 (eternity (cdr l))))))
       (cdr l))))))
 '())
;Value: 0

((lambda (l)
   (cond
    ((null? l) 0)
    (else
     (add1
      ((lambda (l)
	(cond
	 ((null? l) 0)
	 (else
	  (add1 (eternity (cdr l))))))
       (cdr l))))))
 '(1))
;Value: 1

((lambda (l)
   (cond
    ((null? l) 0)
    (else
     (add1
      ((lambda (l)
	(cond
	 ((null? l) 0)
	 (else
	  (add1 (eternity (cdr l))))))
       (cdr l))))))
 '(1 2))

;Quit!

((lambda (l)
   (cond
    ((null? l) 0)
    (else
     (add1
      ((lambda (l)
	 (cond
	  ((null? l) 0)
	  (else
	   (add1
	    ((lambda (l)
	       (cond
		((null? l) 0)
		(else
		 (add1
		  (eternity (cdr l))))))
	     (cdr l))))))
       (cdr l))))))
 '())
;Value: 0

((lambda (l)
   (cond
    ((null? l) 0)
    (else
     (add1
      ((lambda (l)
	 (cond
	  ((null? l) 0)
	  (else
	   (add1
	    ((lambda (l)
	       (cond
		((null? l) 0)
		(else
		 (add1
		  (eternity (cdr l))))))
	     (cdr l))))))
       (cdr l))))))
 '(1))
;Value: 1

((lambda (l)
   (cond
    ((null? l) 0)
    (else
     (add1
      ((lambda (l)
	 (cond
	  ((null? l) 0)
	  (else
	   (add1
	    ((lambda (l)
	       (cond
		((null? l) 0)
		(else
		 (add1
		  (eternity (cdr l))))))
	     (cdr l))))))
       (cdr l))))))
 '(1 2))
;Value: 2



(((lambda (length)
    (lambda (l)
     (cond
      ((null? l) 0)
      (else
       (add1 (length (cdr l)))))))
  eternity)
 '())
;Value: 0

(((lambda (length)
    (lambda (l)
     (cond
      ((null? l) 0)
      (else
       (add1 (length (cdr l)))))))
  eternity)
 '(1))

;Quit!



(((lambda (f)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (f (cdr l)))))))
  ((lambda (g)
     (lambda (l)
       (cond
	((null? l) 0)
	(else (add1 (g (cdr l)))))))
   eternity))
 '())
;Value: 0

(((lambda (f)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (f (cdr l)))))))
  ((lambda (g)
     (lambda (l)
       (cond
	((null? l) 0)
	(else (add1 (g (cdr l)))))))
   eternity))
 '(1))
;Value: 1

(((lambda (f)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (f (cdr l)))))))
  ((lambda (g)
     (lambda (l)
       (cond
	((null? l) 0)
	(else (add1 (g (cdr l)))))))
   eternity))
 '(1 2))

;Quit!



(((lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
       (cond
	((null? l) 0)
	(else (add1 (length (cdr l)))))))
   ((lambda (length)
      (lambda (l)
	(cond
	 ((null? l) 0)
	 (else (add1 (length (cdr l)))))))
    eternity)))
 '())
;Value: 0

(((lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
       (cond
	((null? l) 0)
	(else (add1 (length (cdr l)))))))
   ((lambda (length)
      (lambda (l)
	(cond
	 ((null? l) 0)
	 (else (add1 (length (cdr l)))))))
    eternity)))
 '(1))
;Value: 1

(((lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
       (cond
	((null? l) 0)
	(else (add1 (length (cdr l)))))))
   ((lambda (length)
      (lambda (l)
	(cond
	 ((null? l) 0)
	 (else (add1 (length (cdr l)))))))
    eternity)))
 '(1 2))
;Value: 2

(((lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
       (cond
	((null? l) 0)
	(else (add1 (length (cdr l)))))))
   ((lambda (length)
      (lambda (l)
	(cond
	 ((null? l) 0)
	 (else (add1 (length (cdr l)))))))
    eternity)))
 '(1 2 3))

;Quit!



(((lambda (mk-length)
    (mk-length eternity))
  (lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1 (length (cdr l))))))))
 '())
;Value: 0

(((lambda (mk-length)
    (mk-length eternity))
  (lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1 (length (cdr l))))))))
 '(1))

;Quit!



(((lambda (mk-length)
    (mk-length
     (mk-length eternity)))
  (lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1 (length (cdr l))))))))
 '())
;Value: 0

(((lambda (mk-length)
    (mk-length
     (mk-length eternity)))
  (lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1 (length (cdr l))))))))
 '(1))
;Value: 1

(((lambda (mk-length)
    (mk-length
     (mk-length eternity)))
  (lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1 (length (cdr l))))))))
 '(1 2))

;Quit!



(((lambda (mk-length)
    (mk-length
     (mk-length
      (mk-length eternity))))
  (lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1 (length (cdr l))))))))
 '())
;Value: 0

(((lambda (mk-length)
    (mk-length
     (mk-length
      (mk-length eternity))))
  (lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1 (length (cdr l))))))))
 '(1))
;Value: 1

(((lambda (mk-length)
    (mk-length
     (mk-length
      (mk-length eternity))))
  (lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1 (length (cdr l))))))))
 '(1 2))
;Value: 2

(((lambda (mk-length)
    (mk-length
     (mk-length
      (mk-length eternity))))
  (lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1 (length (cdr l))))))))
 '(1 2 3))

;Quit!



(((lambda (mk-length)
    (mk-length
     (mk-length
      (mk-length
       (mk-length eternity)))))
  (lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1 (length (cdr l))))))))
 '())
;Value: 0

(((lambda (mk-length)
    (mk-length
     (mk-length
      (mk-length
       (mk-length eternity)))))
  (lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1 (length (cdr l))))))))
 '(1))
;Value: 1

(((lambda (mk-length)
    (mk-length
     (mk-length
      (mk-length
       (mk-length eternity)))))
  (lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1 (length (cdr l))))))))
 '(1 2))
;Value: 2

(((lambda (mk-length)
    (mk-length
     (mk-length
      (mk-length
       (mk-length eternity)))))
  (lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1 (length (cdr l))))))))
 '(1 2 3))
;Value: 3

(((lambda (mk-length)
    (mk-length
     (mk-length
      (mk-length
       (mk-length eternity)))))
  (lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1 (length (cdr l))))))))
 '(1 2 3 4))

;Quit!



(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1 (length (cdr l))))))))
 '())
;Value: 0

(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1 (length (cdr l))))))))
 '(1))

;The object #[compound-procedure 18], passed as the first argument to integer-add, is not the correct type.
;To continue, call RESTART with an option number:
; (RESTART 2) => Specify an argument to use in its place.
; (RESTART 1) => Return to read-eval-print level 1.

(RESTART 1)

;Abort!



(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1 (mk-length (cdr l))))))))
 '())
;Value: 0

(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1 (mk-length (cdr l))))))))
 '(1))

;The object #[compound-procedure 23], passed as the first argument to integer-add, is not the correct type.
;To continue, call RESTART with an option number:
; (RESTART 2) => Specify an argument to use in its place.
; (RESTART 1) => Return to read-eval-print level 1.

(RESTART 1)

;Abort!


(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1 ((mk-length eternity)
	       (cdr l))))))))
 '())
;Value: 0



;; --------------------------------------------------
;;
;; process generated in evaluating
;;
;; (((lambda (mk-length)
;;;    (mk-length mk-length))
;;;  (lambda (mk-length)
;;;    (lambda (l)
;;;      (cond
;;;       ((null? l) 0)
;;;       (else
;;;	(add1 ((mk-length eternity)
;;;	       (cdr l))))))))
;;; '(apples))

(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1 ((mk-length eternity)
	       (cdr l))))))))
 '(apples))
;Value: 1

(((lambda (mk-length)
    (mk-length
     (mk-length eternity)))
  (lambda (mk-length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1 (mk-length
	       (cdr l))))))))
 '(apples))
;Value: 1

(((lambda (mk-length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1 (mk-length
	       (cdr l)))))))
 ((lambda (mk-length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1 (mk-length
	       (cdr l)))))))
  eternity))
 '(apples))
;Value: 1

(((lambda (mk-length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1 (mk-length
	       (cdr l)))))))
 (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1 (eternity
	       (cdr l)))))))
 '(apples))
;Value: 1

((lambda (l)
   (cond
    ((null? l) 0)
    (else
     (add1 ((lambda (l)
	      (cond
	       ((null? l) 0)
	       (else
		(add1 (eternity
		       (cdr l))))))
	    (cdr l))))))
 '(apples))
;Value: 1

(cond
    ((null? '(apples)) 0)
    (else
     (add1 ((lambda (l)
	      (cond
	       ((null? '()) 0)
	       (else
		(add1 (eternity
		       (cdr '()))))))
	    '()))))
;Value: 1

(cond
    ((null? '(apples)) 0)
    (else
     (add1 (cond
	       ((null? '()) 0)
	       (else
		(add1 (eternity
		       (cdr '()))))))))
;Value: 1

(cond
    ((null? '(apples)) 0)
    (else
     (add1 0)))
;Value: 1

(add1 0)
;Value: 1

1

;; --------------------------------------------------



(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1 ((mk-length eternity)
	       (cdr l))))))))
 '(1))
;Value: 1



(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1
	 ((mk-length mk-length)
	  (cdr l))))))))
 '())
;Value: 0

(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1
	 ((mk-length mk-length)
	  (cdr l))))))))
 '(1))
;Value: 1

(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1
	 ((mk-length mk-length)
	  (cdr l))))))))
 '(1 2))
;Value: 2

(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else
	(add1
	 ((mk-length mk-length)
	  (cdr l))))))))
 '(1 2 3 4 5 6 7 8 9 10))
;Value: 10



;; --------------------------------------------------
;;
;; process generated in evaluating
;;
;; (((lambda (mk-length)
;;;    (mk-length mk-length))
;;;  (lambda (mk-length)
;;;    ((lambda (length)
;;;       (lambda (l)
;;;	 (cond
;;;	  ((null? l) 0)
;;;	  (else (add1 (length (cdr l)))))))
;;;     (mk-length mk-length))))
;;; '(apples))

(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    ((lambda (length)
       (lambda (l)
	 (cond
	  ((null? l) 0)
	  (else (add1 (length (cdr l)))))))
     (mk-length mk-length))))
 '(apples))

;Aborting!: maximum recursion depth exceeded

(((lambda (mk-length)
    ((lambda (length)
       (lambda (l)
	 (cond
	  ((null? l) 0)
	  (else (add1 (length (cdr l)))))))
     (mk-length mk-length)))
  (lambda (mk-length)
    ((lambda (length)
       (lambda (l)
	 (cond
	  ((null? l) 0)
	  (else (add1 (length (cdr l)))))))
     (mk-length mk-length))))
 '(apples))

;Aborting!: maximum recursion depth exceeded

(((lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
  ((lambda (mk-length)
     ((lambda (length)
	(lambda (l)
	  (cond
	   ((null? l) 0)
	   (else (add1 (length (cdr l)))))))
      (mk-length mk-length)))
   (lambda (mk-length)
     ((lambda (length)
	(lambda (l)
	  (cond
	   ((null? l) 0)
	   (else (add1 (length (cdr l)))))))
      (mk-length mk-length)))))
 '(apples))

;Aborting!: maximum recursion depth exceeded

(((lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
       (cond
	((null? l) 0)
	(else (add1 (length (cdr l)))))))
   ((lambda (mk-length)
      ((lambda (length)
	 (lambda (l)
	   (cond
	    ((null? l) 0)
	    (else (add1 (length (cdr l)))))))
       (mk-length mk-length)))
    (lambda (mk-length)
      ((lambda (length)
	 (lambda (l)
	   (cond
	    ((null? l) 0)
	    (else (add1 (length (cdr l)))))))
       (mk-length mk-length))))))
 '(apples))

;Aborting!: maximum recursion depth exceeded

;; --------------------------------------------------



((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1
	     ((mk-length mk-length)
	      (cdr l))))))))
;Value 28: #[compound-procedure 28]

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1
	     ((lambda (x)
		((mk-length mk-length) x))
	      (cdr l))))))))
;Value 31: #[compound-procedure 31]

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
	(cond
	 ((null? l) 0)
	 (else (add1 (length (cdr l)))))))
    (lambda (x)
      ((mk-length mk-length) x)))))
;Value 32: #[compound-procedure 32]

((lambda (le)
   (lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (le (lambda (x)
	   ((mk-length mk-length) x)))))
 (lambda (length)
      (lambda (l)
	(cond
	 ((null? l) 0)
	 (else (add1 (length (cdr l))))))))
;Value 33: #[compound-procedure 33]

(lambda (le)
   (lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (le (lambda (x)
	   ((mk-length mk-length) x)))))
;Value 34: #[compound-procedure 34]

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))
;Value: y

((Y (lambda (length)
      (lambda (l)
	(cond
	 ((null? l) 0)
	 (else (add1 (length (cdr l))))))))
 '())
;Value: 0

((Y (lambda (length)
      (lambda (l)
	(cond
	 ((null? l) 0)
	 (else (add1 (length (cdr l))))))))
 '(1 2 3 4 5 6 7 8 9 10))
;Value: 10

(Y Y)

;Aborting!: maximum recursion depth exceeded



(lambda (fib)
      (lambda (n)
	(cond
	 ((= n 0) 1)
	 ((= n 1) 1)
	 (else
	  (+ (fib (- n 1))
	     (fib (- n 2)))))))
;Value 13: #[compound-procedure 13]

((Y (lambda (fib)
      (lambda (n)
	(cond
	 ((= n 0) 1)
	 ((= n 1) 1)
	 (else
	  (+ (fib (- n 1))
	     (fib (- n 2))))))))
 10)
;Value: 89

((Y (lambda (fib)
      (lambda (n)
	(cond
	 ((= n 0) 1)
	 ((= n 1) 1)
	 (else
	  (+ (fib (- n 1))
	     (fib (- n 2))))))))
 4)
;Value: 5

