;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;                       Chapter_4.scm
;;                       originated from TLS
;;                       edited by Lawrence R. Amlord(颜世敏 Shi-min Yan)
;;                       informlarry@gmail.com
;;                       Aug 31st, 2013
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
    (and (not (pair? x)) (not (null? x)))))
;Value: atom?

(atom? 14)
;Value: #t

(define add1
  (lambda (n)
    (+ n 1)))
;Value: add1

(add1 67)
;Value: 68

(define sub1
  (lambda (n)
    (- n 1)))
;Value: sub1

(sub1 5)
;Value: 4

(sub1 0)
;Value: -1

(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (o+ n (sub1 m)))))))
;Value: o+

(o+ 46 12)
;Value: 58

(define o2+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (o2+ (add1 n) (sub1 m))))))
;Value: o2+

(o2+ 46 12)
;Value: 58



(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (o- n (sub1 m)))))))
;Value: o-

(o- 14 3)
;Value: 11

(o- 17 9)
;Value: 8

(o- 18 25)
;Value: -7



(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))



(define tup?
  (lambda (t)
    (cond
     ((null? t) #t)
     ((number? (car t)) (tup? (cdr t)))
     (else #f))))
;Value: tup?

(tup? '(2 11 3 79 47 6))
;Value: #t

(tup? '(8 55 5 555))
;Value: #t

(tup? '(1 2 8 apple 4 3))
;Value: #f

(tup? '(3 (7 4) 13 9))
;Value: #f

(tup? '())
;Value: #t



(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else
      (o+ (car tup)
	  (addtup (cdr tup)))))))
;Value: addtup

(addtup '(3 5 2 8))
;Value: 18



(define o*
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else
      (o+ n
	  (o* n (sub1 m)))))))
;Value: o*

(o* 5 3)
;Value: 15

(o* 13 4)
;Value: 52

(o* 12 3)
;Value: 36



(define tup+
  (lambda (tup1 tup2)
    (cond
     ((and (null? tup1) (null? tup2))
      '())
     (else
      (cons (o+ (car tup1) (car tup2))
	    (tup+ (cdr tup1) (cdr tup2)))))))
;Value: tup+

(tup+ '(3 6 9 11 4)
      '(8 5 2 0 7))
;Value 14: (11 11 11 11 11)

(tup+ '(2 3)
      '(4 6))
;Value 15: (6 9)

(tup+ '(3 7)
      '(4 6))
;Value 16: (7 13)

(tup+ '(3 7)
      '(4 6 8 1))

;The object (), passed as the first argument to cdr, is not the correct type.
;To continue, call RESTART with an option number:
; (RESTART 2) => Specify an argument to use in its place.
; (RESTART 1) => Return to read-eval-print level 1.

(RESTART 1)

;Abort!

(define tup+2
  (lambda (tup1 tup2)
    (cond
     ((and (null? tup1) (null? tup2))
      '())
     ((null? tup1) tup2)
     (else
      (cons (o+ (car tup1) (car tup2))
	    (tup+2 (cdr tup1) (cdr tup2)))))))
;Value: tup+2

(tup+2 '(3 7)
       '(4 6 8 1))
;Value 18: (7 13 8 1)

(tup+2 '(4 6 8 1)
       '(3 7))

;The object (), passed as the first argument to cdr, is not the correct type.
;To continue, call RESTART with an option number:
; (RESTART 2) => Specify an argument to use in its place.
; (RESTART 1) => Return to read-eval-print level 1.

(RESTART 1)

;Abort!

(define tup+3
  (lambda (tup1 tup2)
    (cond
     ((and (null? tup1) (null? tup2))
      '())
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else
      (cons (o+ (car tup1) (car tup2))
	    (tup+3 (cdr tup1) (cdr tup2)))))))
;Value: tup+3

(tup+3 '(4 6 8 1)
       '(3 7))
;Value 19: (7 13 8 1)

(define tup+4
  (lambda (tup1 tup2)
    (cond     
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else
      (cons (o+ (car tup1) (car tup2))
	    (tup+4 (cdr tup1) (cdr tup2)))))))
;Value: tup+4



(define o>
  (lambda (n m)
    (cond
     ((zero? m) #t)
     ((zero? n) #f)
     (else (o> (sub1 n) (sub1 m))))))
;Value: o>

(o> 12 133)
;Value: #f

(o> 120 11)
;Value: #t

(o> 3 3)
;Value: #t



(define o2>
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (o2> (sub1 n) (sub1 m))))))
;Value: o2>

(o2> 12 133)
;Value: #f

(o2> 120 11)
;Value: #t

(o2> 3 3)
;Value: #f



(define o<
  (lambda (n m)
    (cond
     ((zero? n) #t)
     ((zero? m) #f)
     (else (o< (sub1 n) (sub1 m))))))
;Value: o<

(o< 4 6)
;Value: #t

(o< 8 3)
;Value: #f

(o< 6 6)
;Value: #t



(define o2<
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (o2< (sub1 n) (sub1 m))))))
;Value: o2<

(o2< 4 6)
;Value: #t

(o2< 8 3)
;Value: #f

(o2< 6 6)
;Value: #f



(define o=
  (lambda (n m)
    (cond
     ((zero? m) (zero? n))
     ((zero? n) #f)
     (else (o= (sub1 n) (sub1 m))))))
;Value: o=

(o= 6 6)
;Value: #t

(o= 3 3)
;Value: #t



(define o2=
  (lambda (n m)
    (cond
     ((o2> n m) #f)
     ((o2< n m) #f)
     (else #t))))
;Value: o2=

(o2= 6 6)
;Value: #t

(o2= 3 3)
;Value: #t



(define ^
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else
      (o* n
	 (^ n (sub1 m)))))))
;Value: ^

(^ 1 1)
;Value: 1

(^ 2 3)
;Value: 8

(^ 5 3)
;Value: 125



(define ???
  (lambda (n m)
    (cond
     ((< n m) 0)
     (else (add1 (??? (o- n m) m))))))
;Value: ???

(??? 1 1)
;Value: 1

(??? 2 1)
;Value: 2

(??? 2 3)
;Value: 0

(??? 3 2)
;Value: 1

(??? 2 4)
;Value: 0

(??? 4 2)
;Value: 2

(??? 4 8)
;Value: 0

(??? 8 4)
;Value: 2

(??? 12 4)
;Value: 3

(define o/
  (lambda (n m)
    (cond
     ((< n m) 0)
     (else (add1 (o/ (o- n m) m))))))
;Value: o/

(o/ 15 4)
;Value: 3



(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else
      (add1 (length (cdr lat)))))))
;Value: length

(length '(hotdogs with mustard sauerkraut and pickles))
;Value: 6

(length '(ham and cheese on rye))
;Value: 5



(define pick
  (lambda (n lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((zero? (sub1 n)) (car lat))
       (else (pick (sub1 n) (cdr lat))))))))
;Value: pick

(pick 4 '(lasagna spaghetti ravioli macaroni meatball))
;Value: macaroni

(pick 0 '(a))
;Value: ()



(define rempick
  (lambda (n lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((zero? (sub1 n)) (cdr lat))
       (else
	(cons (car lat)
	      (rempick (sub1 n) (cdr lat)))))))))
;Value: rempick

(rempick 3 '(hotdogs with hot mustard))
;Value 26: (hotdogs with mustard)



(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((number? (car lat))
	(no-nums (cdr lat)))
       (else (cons (car lat)
		   (no-nums (cdr lat)))))))))
;Value: no-number

(no-number '(5 pears 6 prunes 9 dates))
;Value 23: (pears prunes dates)



(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((number? (car lat))
	(cons (car lat)
	      (all-nums (cdr lat))))
       (else (all-nums (cdr lat))))))))
;Value: all-nums

(all-nums '(5 pears 6 prunes 9 dates))
;Value 24: (5 6 9)



(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2))
      (= a1 a2))
     ((and (atom? a1) (atom? a2))
      (eq? a1 a2))
     (else #f))))
;Value: eqan?

(eqan? 0 0)
;Value: #t

(eqan? 8 8)
;Value: #t

(eqan? 1 5)
;Value: #f

(eqan? 'ice 'ice)
;Value: #t

(eqan? 'ice 'cream)
;Value: #f

(eqan? '(cream) 'cream)
;Value: #f

(eqan? '7 'orange)
;Value: #f



(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     (else
      (cond
       ((eqan? (car lat) a)
	(add1 (occur a (cdr lat))))
       (else
	(occur a (cdr lat))))))))
;Value: occur

(occur 'apple '(An apple a day, keeps doctor away!))
;Value: 1

(occur 'be '(To be or not to be, that is a question.))
;Value: 2



(define one?
  (lambda (n)
    (= n 1)))
;Value: one?



(define rempick2
  (lambda (n lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((one? n) (cdr lat))
       (else
	(cons (car lat)
	      (rempick2 (sub1 n) (cdr lat)))))))))
;Value: rempick2

(rempick2 3 '(lemon meringue salty pie))
;Value 27: (lemon meringue pie)

