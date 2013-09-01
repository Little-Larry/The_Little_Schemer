;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;                       Chapter_7.scm
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



(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     (else
      (cond
       ((member? (car lat) (cdr lat)) #f)
       (else
	(set? (cdr lat))))))))
;Value: set?

(set? '(apple peaches apple plum))
;Value: #f

(set? '(apples peaches pears plums))
;Value: #t



(define set2?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else
      (set2? (cdr lat))))))
;Value: set2?

(set2? '(apple peaches apple plum))
;Value: #f

(set2? '(apples peaches pears plums))
;Value: #t

(set2? '(apple 3 pear 4 9 apple 3 4))
;Value: #f

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else
      (or (equal? (car lat) a)
	  (member? a (cdr lat)))))))
;Value: member?

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

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((member? (car lat) (cdr lat))
      (makeset (cdr lat)))
     (else
      (cons (car lat)
	    (makeset (cdr lat)))))))
;Value: makeset

(makeset '(apple peach pear peach plum apple lemon peach))
;Value 23: (pear plum apple lemon peach)



(define makeset2
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else
      (cons (car lat)
	    (makeset2 (multirember (car lat)
				   (cdr lat))))))))
;Value: makeset2

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((equal? (car lat) a)
      (multirember a (cdr lat)))
     (else
      (cons (car lat)
	    (multirember a (cdr lat)))))))
;Value: multirember

(makeset2 '(apple peach pear peach plum apple lemon peach))
;Value 24: (apple peach pear plum lemon)

(makeset2 '(apple 3 4 pear 9 apple 3 4))
;Value 25: (apple 3 4 pear 9)



(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     ((member? (car set1) set2)
      (subset? (cdr set1) set2))
     (else #f))))
;Value: subset?

(subset? '(5 chicken wings)
	 '(5 hamburgers 2 pieces fried chicken and light duckling wings))
;Value: #t

(subset? '(4 pounds of horseradish)
	 '(four pounds chicken and 5 ounces horseradish))
;Value: #f

(define subset2?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else 
      (and (member? (car set1) set2)
	   (subset2? (cdr set1) set2))))))
;Value: subset2?

(subset2? '(5 chicken wings)
	 '(5 hamburgers 2 pieces fried chicken and light duckling wings))
;Value: #t

(subset2? '(4 pounds of horseradish)
	 '(four pounds chicken and 5 ounces horseradish))
;Value: #f



(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
	 (subset? set2 set1))))
;Value: eqset?

(eqset? '(6 large chickens with wings)
	'(6 chickens with large wings))
;Value: #t



(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else
      (cond
       ((member? (car set1) set2) #t)
       (else
	(intersect? (cdr set1) set2)))))))
;Value: intersect?

(intersect? '(stewed tomatoes and macaroni)
	    '(macaroni and cheese))
;Value: #t

(define intersect2?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     ((member? (car set1) set2) #t)
     (else
      (intersect2? (cdr set1) set2)))))
;Value: intersect2?

(intersect2? '(stewed tomatoes and macaroni)
	     '(macaroni and cheese))
;Value: #t

(define intersect3?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else
      (or (member? (car set1) set2) 
	  (intersect3? (cdr set1) set2))))))
;Value: intersect3?

(intersect3? '(stewed tomatoes and macaroni)
	     '(macaroni and cheese))
;Value: #t



(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2)
      (cons (car set1)
	    (intersect (cdr set1) set2)))
     (else
      (intersect (cdr set1) set2)))))
;Value: intersect

(intersect '(stewed tomatoes and macaroni)
	   '(macaroni and cheese))
;Value 29: (and macaroni)



(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2)
      (union (cdr set1) set2))
     (else
      (cons (car set1)
	    (union (cdr set1) set2))))))
;Value: union

(union '(stewed tomatoes and macaroni casserole)
       '(macaroni and cheese))
;Value 30: (stewed tomatoes casserole macaroni and cheese)



(define xxx
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2)
      (xxx (cdr set1) set2))
     (else
      (cons (car set1)
	    (xxx (cdr set1) set2))))))
;Value: xxx

(xxx '(stewed tomatoes and macaroni casserole)
     '(macaroni and cheese))
;Value 31: (stewed tomatoes casserole)



(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else
      (intersect (car l-set)
		 (intersectall (cdr l-set)))))))
;Value: intersectall

(intersectall '((a b c) (c a d e) (e f g h a b)))
;Value 32: (a)

(intersectall '((6 pear and)
		(3 peaches and 6 peppers)
		(8 pears and 6 plums)
		(and 6 prunes with some apples)))
;Value 33: (6 and)



(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))
;Value: a-pair?

(a-pair? '(pear pear))
;Value: #t

(a-pair? '(3 7))
;Value: #t

(a-pair? '((2) (pair)))
;Value: #t

(a-pair? '(full (house)))
;Value: #t



(define first
  (lambda (p)
    (car p)))
;Value: first

(define second
  (lambda (p)
    (car (cdr p))))
;Value: second

(define build
  (lambda (s1 s2)
    (cons s1
	  (cons s2 '()))))
;Value: build



(define fun?
  (lambda (rel)
    (set? (firsts rel))))
;Value: fun?

(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else
      (cons (car (car l))
	    (firsts (cdr l)))))))
;Value: firsts

(fun? '((4 3) (4 2) (7 6) (6 2) (3 4)))
;Value: #f

(fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
;Value: #t

(fun? '((d 4) (b 0) (b 9) (e 5) (g 4)))
;Value: #f



(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else
      (cons (build (second (car rel))
		   (first (car rel)))
	    (revrel (cdr rel)))))))
;Value: revrel

(revrel '((8 a) (pumpkin pie) (got sick)))
;Value 34: ((a 8) (pie pumpkin) (sick got))



(define revrel2
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else
      (cons (revpair (car rel))
	    (revrel2 (cdr rel)))))))
;Value: revrel2

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))
;Value: revpair

(revrel2 '((8 a) (pumpkin pie) (got sick)))
;Value 35: ((a 8) (pie pumpkin) (sick got))



(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))
;Value: fullfun?

(define seconds
  (lambda (l)
    (cond
     ((null? l) '())
     (else
      (cons (car (cdr (car l)))
	   (seconds (cdr l)))))))
;Value: seconds

(seconds '((apple banana) (pear orange) (mustarsh mushroom) (juice ice cream)))
;Value 36: (banana orange mushroom ice)

(fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
;Value: #f

(fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4)))
;Value: #t

(fullfun? '((grape reisin)
	    (plum prune)
	    (stewed prune)))
;Value: #f

(fullfun? '((grape reisin)
	    (plum prune)
	    (stewed grape)))
;Value: #t

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))
;Value: one-to-one?

(one-to-one? '((chocolate chip) (doughy cookie)))
;Value: #t

