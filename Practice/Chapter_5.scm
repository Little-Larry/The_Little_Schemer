;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;                       Chapter_5.scm
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
    (and (not (null? x)) (not (pair? x)))))
;Value: atom?

(define add1
  (lambda (n)
    (+ n 1)))
;Value: add1

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2))
      (= a1 a2))
     ((or (number? a1) (number? a2))
      #f)
     (else (eq? a1 a2)))))
;Value: eqan?

(define rember*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) a)
	(rember* a (cdr l)))
       (else
	(cons (car l)
	      (rember* a (cdr l))))))
     (else
      (cons (rember* a (car l))
	    (rember* a (cdr l)))))))
;Value: rember*

(rember* 'cup '((coffee)
		cup
		((tea) cup)
		(and (hick))
		cup))
;Value 13: ((coffee) ((tea)) (and (hick)))

(rember* 'sauce '(((tomato sauce))
		  ((bean) sauce)
		  (and ((flying)) suace)))
;Value 14: (((tomato)) ((bean)) (and ((flying)) suace))



(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
	(cons old
	      (cons new
		    (insertR* new old (cdr l)))))
       (else
	(cons (car l)
	      (insertR* new old (cdr l))))))
     (else
      (cons (insertR* new old (car l))
	    (insertR* new old (cdr l)))))))
;Value: insertr*

(insertR* 'roast 'chuck '((how much (wood))
			  could
			  ((a (wood) chuck))
			  (((chuck)))
			  (if (a) ((wood chuck)))
			  could chuck wood))
;Value 15: ((how much (wood)) could ((a (wood) chuck roast)) (((chuck roast))) (if (a) ((wood chuck roast))) could chuck roast wood)



(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? (car l) a)
	(add1 (occur* a (cdr l))))
       (else
	(occur* a (cdr l)))))
     (else
      (+ (occur* a (car l))
	 (occur* a (cdr l)))))))
;Value: occur*

(occur* 'banana '((banana)
		  (split ((((banana ice)))
			  (cream (banana))
			  sherbet))
		  (banana)
		  (bread)
		  (banana brandy)))
;Value: 5



(define subst*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
	(cons new (cdr l)))
       (else
	(cons (car l)
	      (subst* new old (cdr l))))))
     (else
      (cons (subst* new old (car l))
	    (subst* new old (cdr l)))))))
;Value: subst*

(subst* 'orange 'banana '((banana)
			  (split ((((banana ice)))
				  (cream (banana))
				  sherbet))
			  (banana)
			  (bread)
			  (banana brandy)))
;Value 16: ((orange) (split ((((orange ice))) (cream (orange)) sherbet)) (orange) (bread) (orange brandy))



(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
	(cons new
	      (cons old
		    (insertL* new old (cdr l)))))
       (else
	(cons (car l)
	      (insertL* new old (cdr l))))))
     (else
      (cons (insertL* new old (car l))
	    (insertL* new old (cdr l)))))))
;Value: insertl*

(insertL* 'pecker 'chuck '((how much (wood))
			   could
			   ((a (wood) chuck))
			   (((chuck)))
			   (if (a) ((wood chuck)))
			   could chuck wood))
;Value 17: ((how much (wood)) could ((a (wood) pecker chuck)) (((pecker chuck))) (if (a) ((wood pecker chuck))) could pecker chuck wood)



(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l))
      (cond
       ((eq? (car l) a) #t)
       (else (member* a (cdr l)))))
     (else
      (cond
       ((member* a (car l)) #t)
       (else (member* a (cdr l))))))))
;Value: member*

(member* 'chips '((potato) (chips ((with) fish) (chips))))
;Value: #t



(define member*2
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l))
      (or (eq? (car l) a)
	  (member*2 a (cdr l))))
     (else
      (or (member*2 a (car l))
	  (member*2 a (cdr l)))))))
;Value: member*2

(member*2 'chips '((potato) (chips ((with) fish) (chips))))
;Value: #t



(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))
;Value: leftmost

(leftmost '((potato) (chips ((with) fish) (chips))))
;Value: potato

(leftmost '(((hot) (tuna (and))) cheese))
;Value: hot

(leftmost '(((() four)) 17 (seventeen)))

;The object (), passed as the first argument to car, is not the correct type.
;To continue, call RESTART with an option number:
; (RESTART 2) => Specify an argument to use in its place.
; (RESTART 1) => Return to read-eval-print level 1.

(RESTART 1)

;Abort!



(define x 'pizza)
;Value: x

(define l '(mozzarella pizza))
;Value: l

(and (atom? (car l))
     (eq? (car l) x))
;Value: #f

(define l2 '((mozzarella mushroom) pizza))
;Value: l2

(and (atom? (car l2))
     (eq? (car l2) x))
;Value: #f

(define l3 '(pizza (tastes good)))
;Value: l3




(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((and (null? l1) (atom? (car l2)))
      #f)
     ((null? l1) #f)
     ((and (atom? (car l1)) (null? l2))
      #f)
     ((and (atom? (car l1))
	   (atom? (car l2)))
      (and (eqan? (car l1) (car l2))
	   (eqlist? (cdr l1) (cdr l2))))
     ((atom? (car l1)) #f)
     ((null? l2) #f)
     ((atom? (car l2)) #f)
     (else
      (and (eqlist? (car l1) (car l2))
	   (eqlist? (cdr l1) (cdr l2)))))))
;Value: eqlist?

(eqlist? '(strawberry ice cream) '(strawberry ice cream))
;Value: #t

(eqlist? '(strawberry ice cream) '(strawberry cream ice))
;Value: #f

(eqlist? '(banana ((split))) '((banana) (split)))
;Value: #f

(eqlist? '(beef ((sausage)) (and (soda)))
	 '(beef ((salami)) (and (soda))))
;Value: #f

(eqlist? '(beef ((sausage)) (and (soda)))
	 '(beef ((sausage)) (and (soda))))
;Value: #t

(define eqlist2?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     ((and (atom? (car l1))
	   (atom? (car l2)))
      (and (eqan? (car l1) (car l2))
	   (eqlist2? (cdr l1) (cdr l2))))
     ((or (atom? (car l1)) (atom? (car l2))) #f)
     (else
      (and (eqlist2? (car l1) (car l2))
	   (eqlist2? (cdr l1) (cdr l2)))))))
;Value: eqlist2?



(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2))
      (eqan? s1 s2))
     ((atom? s1) #f)
     ((atom? s2) #f)
     (else
      (eqlist? s1 s2)))))
;Value: equal?

(define equal2?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2))
      (eqan? s1 s2))
     ((or (atom? s1) (atom? s2)) #f)
     (else
      (eqlist? s1 s2)))))
;Value: equal2?

(define eqlist3?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else
      (and (equal2? (car l1) (car l2))
	   (eqlist3? (cdr l1) (cdr l2)))))))
;Value: eqlist3?

(eqlist3? '(strawberry ice cream) '(strawberry ice cream))
;Value: #t

(eqlist3? '(strawberry ice cream) '(strawberry cream ice))
;Value: #f

(eqlist3? '(banana ((split))) '((banana) (split)))
;Value: #f

(eqlist3? '(beef ((sausage)) (and (soda)))
	 '(beef ((salami)) (and (soda))))
;Value: #f

(eqlist3? '(beef ((sausage)) (and (soda)))
	 '(beef ((sausage)) (and (soda))))
;Value: #t



(define rember
  (lambda (s l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((equal? (car l) s) (cdr l))
       (else
	(cons (car l)
	      (rember s (cdr l))))))
     (else
      (cond
       ((equal? (car l) s) (cdr l))
       (else
	(cons (car l)
	      (rember s (cdr l)))))))))
;Value: rember

(define rember2
  (lambda (s l)
    (cond
     ((null? l) '())
     (else
      (cond
       ((equal? (car l) s) (cdr l))
       (else
	(cons (car l)
	      (rember2 s (cdr l)))))))))
;Value: rember2

(define rember3
  (lambda (s l)
    (cond
     ((null? l) '())
     ((equal? (car l) s) (cdr l))
     (else
      (cons (car l)
	    (rember3 s (cdr l)))))))
;Value: rember3

