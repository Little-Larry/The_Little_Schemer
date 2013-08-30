;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;                       Chapter_3.scm
;;                       originated from TLS
;;                       edited by Lawrence R. Amlord(颜世敏 Shi-min Yan)
;;                       informlarry@gmail.com
;;                       Aug 30th, 2013
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

(define rember*
  (lambda (a lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) a)
	(cdr lat))
       (else
	(rember* a (cdr lat))))))))
;Value: rember*

(rember* 'bacon '(bacon lettuce and tomato))
;Value 19: (lettuce and tomato)

(rember* 'and '(bacon lettuce and tomato))
;Value 20: (tomato)

(rember 'and '(bacon lettuce and tomato))
;Value 21: (bacon lettuce tomato)



(define rember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) a)
	(cdr lat))
       (else
	(cons (car lat)
	      (rember a (cdr lat)))))))))
;Value: rember

(rember 'mint '(lamb chops and mint jelly))
;Value 13: (lamb chops and jelly)

(rember 'mint '(lamb chops and mint flavored mint jelly))
;Value 14: (lamb chops and flavored mint jelly)

(rember 'toast '(bacon lettuce and tomato))
;Value 15: (bacon lettuce and tomato)

(rember 'cup '(coffee cup tea cup and hick cup))
;Value 16: (coffee tea cup and hick cup)



(define rember2
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a) (cdr lat))
     (else
      (cons (car lat)
	    (rember2 a (cdr lat)))))))
;Value: rember2

(rember2 'and '(bacon lettuce and tomato))
;Value 22: (bacon lettuce tomato)

(rember2 'sauce '(soy sauce and tomato sauce))
;Value 23: (soy and tomato sauce)



(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else
      (cons (car (car l))
	    (firsts (cdr l)))))))
;Value: firsts

(firsts '((apple peach pumpkin)
	  (plum pear cherry)
	  (grape raisin pea)
	  (bean carrot eggplant)))
;Value 24: (apple plum grape bean)

(firsts '((a b) (c d) (e f)))
;Value 25: (a c e)

(firsts '())
;Value: ()

(firsts '((five plums)
	  (four)
	  (eleven green oranges)))
;Value 26: (five four eleven)

(firsts '(((five plums) four)
	  (eleven green oranges)
	  ((no) more)))
;Value 27: ((five plums) eleven (no))



(define insertR*
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old) (cdr lat))
       (else
	(cons (car lat)
	      (insertR* new old (cdr lat)))))))))
;Value: insertr*

(insertR* 'topping 'fudge '(ice cream with fudge for dessert))
;Value 32: (ice cream with for dessert)



(define insertR**
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old)
	(cons new (cdr lat)))
       (else
	(cons (car lat)
	      (insertR** new old (cdr lat)))))))))
;Value: insertr**

(insertR** 'topping 'fudge '(ice cream with fudge for dessert))
;Value 34: (ice cream with topping for dessert)



(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old)
	(cons old
	      (cons new (cdr lat))))
       (else
	(cons (car lat)
	      (insertR new old (cdr lat)))))))))
;Value: insertr

(insertR 'topping 'fudge '(ice cream with fudge for dessert))
;Value 36: (ice cream with fudge topping for dessert)

(insertR 'jalapeno 'and '(tacos tamales and salsa))
;Value 37: (tacos tamales and jalapeno salsa)

(insertR 'e 'd '(a b c d f g d h))
;Value 38: (a b c d e f g d h)



(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old)
	(cons new lat))
       (else
	(cons (car lat)
	      (insertL new old (cdr lat)))))))))
;Value: insertl

(insertL 'topping 'fudge '(ice cream with fudge for dessert))
;Value 39: (ice cream with topping fudge for dessert)

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old)
	(cons new (cdr lat)))
       (else
	(cons (car lat)
	      (subst new old (cdr lat)))))))))
;Value: subst

(subst 'topping 'fudge '(ice cream with fudge for dessert))
;Value 40: (ice cream with topping for dessert)

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((or (eq? (car lat) o1) (eq? (car lat) o2))
	(cons new (cdr lat)))
       (else
	(cons (car lat)
	      (subst new old (cdr lat)))))))))
;Value: subst2

(subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))
;Value 41: (vanilla ice cream with chocolate topping)



(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) a)
	(multirember a (cdr lat)))
       (else
	(cons (car lat)
	      (multirember a (cdr lat)))))))))
;Value: multirember

(multirember 'cup '(coffee cup tea cup and hick cup))
;Value 42: (coffee tea and hick)



(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old)
	(cons old
	      (cons new
		    (multiinsertR new old (cdr lat)))))
       (else
	(cons (car lat)
	      (multiinsertR new old (cdr lat)))))))))
;Value: multiinsertr

(multiinsertR 'candy 'cup '(coffee cup tea cup and hick cup))
;Value 44: (coffee cup candy tea cup candy and hick cup candy)

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old)
	(cons new
	      (cons old
		    (multiinsertL new old (cdr lat)))))
       (else
	(cons (car lat)
	      (multiinsertL new old (cdr lat)))))))))
;Value: multiinsertl

(multiinsertL 'crystal 'cup '(coffee cup tea cup and hick cup))
;Value 45: (coffee crystal cup tea crystal cup and hick crystal cup)

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old)
	(cons new
	      (multisubst new old (cdr lat))))
       (else
	(cons (car lat)
	      (multisubst new old (cdr lat)))))))))
;Value: multisubst

(multisubst 'candy 'cup '(coffee cup tea cup and hick cup))
;Value 46: (coffee candy tea candy and hick candy)

