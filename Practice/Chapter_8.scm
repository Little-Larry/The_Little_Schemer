;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;                       Chapter_8.scm
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

(define ^
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else
      (o* n (^ n (sub1 m)))))))
;Value: ^

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2))
      (= a1 a2))
     ((or (number? a1) (number? a2)) #f)
     (else
      (eq? a1 a2)))))
;Value: eqan?

(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2))
      (eqan? s1 s2))
     ((or (atom? s1) (atom? s2))
      #f)
     (else
      (eqlist? s1 s2)))))
;Value: equal?

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else
      (and (equal? (car l1) (car l2))
	   (eqlist? (cdr l1) (cdr l2)))))))
;Value: eqlist?

(define rember-f
  (lambda (test? a l)
    (cond
     ((null? l) '())
     (else
      (cond
       ((test? (car l) a) (cdr l))
       (else
	(cons (car l)
	      (rember-f test? a (cdr l)))))))))
;Value: rember-f

(rember-f = 5 '(6 2 5 3))
;Value 13: (6 2 3)

(rember-f eq? 'jelly '(jelly beans are good))
;Value 14: (beans are good)

(rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake)))
;Value 15: (lemonade and (cake))



(define rember-f2
  (lambda (test? a l)
    (cond
     ((null? l) '())
     ((test? (car l) a)
      (cdr l))
     (else
      (cons (car l)
	    (rember-f2 test? a (cdr l)))))))
;Value: rember-f2

(rember-f2 = 5 '(6 2 5 3))
;Value 16: (6 2 3)

(rember-f2 eq? 'jelly '(jelly beans are good))
;Value 17: (beans are good)

(rember-f2 equal? '(pop corn) '(lemonade (pop corn) and (cake)))
;Value 18: (lemonade and (cake))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))
;Value: eq?-c

(define eq?-salad (eq?-c 'salad))
;Value: eq?-salad

(eq?-salad 'salad)
;Value: #t

(eq?-salad 'tuna)
;Value: #f

((eq?-c 'salad) 'tuna)
;Value: #f

(define rember-f3
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? (car l) a) (cdr l))
       (else
	(cons (car l)
	      ((rember-f3 test?) a (cdr l))))))))
;Value: rember-f3

((rember-f3 =) 5 '(6 2 5 3))
;Value 19: (6 2 3)

((rember-f3 eq?) 'jelly '(jelly beans are good))
;Value 20: (beans are good)

((rember-f3 equal?) '(pop corn) '(lemonade (pop corn) and (cake)))
;Value 21: (lemonade and (cake))

((rember-f3 eq?) 'tuna '(shrimp salad and tuna salad))
;Value 22: (shrimp salad and salad)

((rember-f3 eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?))
;Value 23: (equal? eqan? eqlist? eqpair?)



(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? (car l) old)
	(cons new l))
       (else
	(cons (car l)
	      ((insertL-f test?) new old (cdr l))))))))
;Value: insertl-f

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? (car l) old)
	(cons old
	      (cons new
		    (cdr l))))
       (else
	(cons (car l)
	      ((insertR-f test?) new old (cdr l))))))))
;Value: insertr-f



(define seqL
  (lambda (new old l)
    (cons new (cons old l))))
;Value: seql

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))
;Value: seqr

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((eq? (car l) old)
	(seq new old (cdr l)))
       (else
	(cons (car l)
	      ((insert-g seq) new old (cdr l))))))))
;Value: insert-g

((insert-g seqL) 'topping 'fudge '(ice cream with fudge for dessert))
;Value 26: (ice cream with topping fudge for dessert)

((insert-g seqR) 'topping 'fudge '(ice cream with fudge for dessert))
;Value 27: (ice cream with fudge topping for dessert)

(define insertL (insert-g seqL))
;Value: insertl

(define insertR (insert-g seqR))
;Value: insertr

(define insertL2
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))
;Value: insertl2

(insertL2 'topping 'fudge '(ice cream with fudge for dessert))
;Value 28: (ice cream with topping fudge for dessert)

(define insertR2
  (insert-g
   (lambda (new old l)
     (cons old (cons new l)))))
;Value: insertr2

(insertR2 'topping 'fudge '(ice cream with fudge for dessert))
;Value 29: (ice cream with fudge topping for dessert)



(define seqS
  (lambda (new old l)
    (cons new l)))
;Value: seqs

(define subst (insert-g seqS))
;Value: subst

(subst 'topping 'fudge '(ice cream with fudge for dessert))
;Value 30: (ice cream with topping for dessert)



(define yyy
  (lambda (a l)
    ((insert-g seqrem) #f a l)))
;Value: yyy

(define seqrem
  (lambda (new old l)
    l))
;Value: seqrem

(yyy 'sausage '(pizza with sausage and bacon))
;Value 31: (pizza with and bacon)

;; Process generated in evaluating (yyy a l)
(yyy 'sausage '(pizza with sausage and bacon))
((insert-g (lambda (new old l) l)) #f 'sausage '(pizza with sausage and bacon))
(cons 'pizza
      ((insert-g (lambda (new old l) l)) #f 'sausage '(with sausage and bacon)))
(cons 'pizza
      (cons 'with
	    ((insert-g (lambda (new old l) l)) #f 'sausage '(sausage and bacon))))
(cons 'pizza
      (cons 'with
	    ((lambda (new old l) l) #f 'sausage '(and bacon))))
(cons 'pizza
      (cons 'with
	    '(and bacon)))
(pizza with and bacon)



(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) '+)
      (o+ (value (1st-sub-exp nexp))
	  (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) '*)
      (o* (value (1st-sub-exp nexp))
	  (value (2nd-sub-exp nexp))))
     (else
      (^ (value (1st-sub-exp nexp))
	 (value (2nd-sub-exp nexp)))))))
;Value: value

(define operator
  (lambda (aexp)
    (car aexp)))
;Value: operator

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))
;Value: 1st-sub-exp

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))
;Value: 2nd-sub-exp

(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x '+) o+)
     ((eq? x '*) o*)
     (else ^))))
;Value: atom-to-function

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else
      ((atom-to-function (operator nexp))
       (value (1st-sub-exp nexp))
       (value (2nd-sub-exp nexp)))))))
;Value: value

(value '(+ 2 3))
;Value: 5

(value '(* 2 3))
;Value: 6

(value '(^ 2 3))
;Value: 8



(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a)
      (multirember a (cdr lat)))
     (else
      (cons (car lat)
	    (multirember a (cdr lat)))))))
;Value: multirember

(multirember 'tuna '(shrimp salad tuna salad and tuna))
;Value 35: (shrimp salad salad and)

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) '())
       ((test? (car lat) a)
	((multirember-f test?) a (cdr lat)))
       (else
	(cons (car lat)
	      ((multirember-f test?) a (cdr lat))))))))
;Value: multirember-f

((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))
;Value 36: (shrimp salad salad and)

(define eq?-tuna
  (eq?-c 'tuna))
;Value: eq?-tuna

(define eq?-c
  (lambda (x)
    (lambda (c)
      (eq? x c))))
;Value: eq?-c

(define multiremberT
  (lambda (test? lat)
    (cond
     ((null? lat) '())
     ((test? (car lat))
      (multiremberT test? (cdr lat)))
     (else
      (cons (car lat)
	    (multiremberT test? (cdr lat)))))))
;Value: multirembert

(multiremberT eq?-tuna '(shrimp salad tuna salad and tuna))
;Value 37: (shrimp salad salad and)




(define multiremberEco
  (lambda (a lat col)
    (cond
     ((null? lat)
      (col '() '()))
     ((eq? (car lat) a)
      (multiremberEco a
		      (cdr lat)
		      (lambda (newlat seen)
			(col newlat
			     (cons (car lat) seen)))))
     (else
      (multiremberEco a
		      (cdr lat)
		      (lambda (newlat seen)
			(col (cons (car lat) newlat)
			     seen)))))))
;Value: multirembereco

(define a-friend
  (lambda (x y) (null? y)))
;Value: a-friend

(multiremberEco 'tuna
		'(strawberries tuna and swordfish)
		a-friend)
;Value: #f

(multiremberEco 'tuna
		'()
		a-friend)
;Value: #t

(multiremberEco 'tuna
		'(tuna)
		a-friend)
;Value: #f

(define last-friend
  (lambda (x y)
    (length x)))
;Value: last-friend

(multiremberEco 'tuna
		'(strawberries tuna and swordfish)
		last-friend)
;Value: 3



(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old)
      (cons new
	    (cons old
		  (multiinsertL new old (cdr lat)))))
     (else
      (cons (car lat)
	    (multiinsertL new old (cdr lat)))))))
;Value: multiinsertl

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old)
      (cons old
	    (cons new
		  (multiinsertR new old (cdr lat)))))
     (else
      (cons (car lat)
	    (multiinsertR new old (cdr lat)))))))
;Value: multiinsertr

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) oldL)
      (cons new
	    (cons oldL
		  (multiinsertLR new oldL oldR (cdr lat)))))
     ((eq? (car lat) oldR)
      (cons oldR
	    (cons new
		  (multiinsertLR new oldL oldR (cdr lat)))))
     (else
      (cons (car lat)
	    (multiinsertLR new oldL oldR (cdr lat)))))))
;Value: multiinsertlr

(define multiinsertLREco
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat)
      (col '() 0 0))
     ((eq? (car lat) oldL)
      (multiinsertLREco new oldL oldR (cdr lat)
			(lambda (newlat L R)
			  (col (cons new
				     (cons oldL newlat))
			       (add1 L) R))))
     ((eq? (car lat) oldR)
      (multiinsertLREco new oldL oldR (cdr lat)
			(lambda (newlat L R)
			  (col (cons oldR
				     (cons new newlat))
			       L (add1 R)))))
     (else
      (multiinsertLREco new oldL oldR (cdr lat)
			(lambda (newlat L R)
			  (col (cons (car lat) newlat)
			       L R)))))))
;Value: multiinsertlreco



(define even?
  (lambda (n)
    (= (remainder n 2) 0)))
;Value: even?

(define evens-only*
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((even? (car l))
	(cons (car l)
	      (evens-only* (cdr l))))
       (else (evens-only* (cdr l)))))
     (else
      (cons (evens-only* (car l))
	    (evens-only* (cdr l)))))))
;Value: evens-only*

(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))
;Value 39: ((2 8) 10 (() 6) 2)

(define evens-only*Eco
  (lambda (l col)
    (cond
     ((null? l)
      (col '() 1 0))
     ((atom? (car l))
      (cond
       ((even? (car l))
	(evens-only*Eco (cdr l)
			(lambda (newl p s)
			  (col (cons (car l) newl)
			       (* (car l) p)
			       s))))
       (else
	(evens-only*Eco (cdr l)
			(lambda (newl p s)
			  (col newl p (+ (car l) s)))))))
     (else
      (evens-only*Eco (car l)
		      (lambda (al ap as)
			(evens-only*Eco (cdr l)
					(lambda (dl dp ds)
					  (col (cons al dl)
					       (* ap dp)
					       (+ as ds))))))))))
;Value: evens-only*eco

(define the-last-friend
  (lambda (newl product sum)
    (cons sum
	  (cons product newl))))
;Value: the-last-friend

(evens-only*Eco '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
		the-last-friend)
;Value 40: (38 1920 (2 8) 10 (() 6) 2)

