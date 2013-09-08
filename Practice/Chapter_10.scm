;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;                       Chapter_10.scm
;;                       originated from TLS
;;                       edited by Lawrence R. Amlord(颜世敏 Shi-min Yan)
;;                       informlarry@gmail.com
;;                       Sept 6th, 2013
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

(define third
  (lambda (p)
    (car (cdr (cdr p)))))
;Value: third

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

(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x '+) +)
     ((eq? x '*) *)
     (else expt))))
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

(define new-entry build)
;Value: new-entry

(define E1 (new-entry '(appetizer entree beverage)
		      '(pate boeuf vin)))
;Value: e1

(display E1)
((appetizer entree beverage) (pate boeuf vin))
;Unspecified return value

(define E2 (new-entry '(appetizer entree beverage)
		      '(beer beer beer)))
;Value: e2

(display E2)
((appetizer entree beverage) (beer beer beer))
;Unspecified return value

(define E3 (new-entry '(beverage dessert)
		      '((food is) (number one with us))))
;Value: e3

(display E3)
((beverage dessert) ((food is) (number one with us)))
;Unspecified return value

(define signal-inexistent
  (lambda (arg)
    (error "Element not found --" arg)))
;Value: signal-inexistent

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
			  (first entry)
			  (second entry)
			  entry-f)))
;Value: lookup-in-entry

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
     ((null? names) (entry-f name))
     ((eq? (car names) name)
      (car values))
     (else
      (lookup-in-entry-help name
			    (cdr names)
			    (cdr values)
			    entry-f)))))
;Value: lookup-in-entry-help

(lookup-in-entry 'entree 
		 '((appetizer entree beverage)
		   (food tastes good))
		 signal-inexistent)
;Value: tastes

(lookup-in-entry 'dessert
		 '((appetizer entree beverage)
		   (food tastes good))
		 signal-inexistent)

;Element not found -- dessert
;To continue, call RESTART with an option number:
; (RESTART 1) => Return to read-eval-print level 1.

(RESTART 1)

;Abort!



(define extend-table cons)
;Value: extend-table

(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else
      (lookup-in-entry name
		       (car table)
		       (lambda (name)
			 (lookup-in-table name
					  (cdr table)
					  table-f)))))))
;Value: lookup-in-table

(lookup-in-table 'entree
		 '(((entree dessert) (spaghetti spumoni))
		   ((appetizer entree beverage) (food tastes good)))
		 signal-inexistent)
;Value: spaghetti

(car '(a b c))
;Value: a

(cons 'a
      (cons 'b
	    (cons 'c '())))
;Value 13: (a b c)

(cons 'car
      (cons (cons 'quote
		  (cons (cons 'a
			      (cons 'b
				    (cons 'c '())))
			'()))
	    '()))
;Value 14: (car (quote (a b c)))

(car '(a b c))
;Value: a



(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))
;Value: expression-to-action

(define atom-to-action
  (lambda (e)
    (cond
     ((number? e) *const)
     ((eq? e #t) *const)
     ((eq? e #f) *const)
     ((eq? e (quote cons)) *const)
     ((eq? e (quote car)) *const)
     ((eq? e (quote cdr)) *const)
     ((eq? e (quote null?)) *const)
     ((eq? e (quote eq?)) *const)
     ((eq? e (quote atom?)) *const)
     ((eq? e (quote zero?)) *const)
     ((eq? e (quote add1)) *const)
     ((eq? e (quote sub1)) *const)
     ((eq? e (quote number?)) *const)
     (else *identifier))))
;Value: atom-to-action

(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond
       ((eq? (car e) (quote quote))
	*quote)
       ((eq? (car e) (quote lambda))
	*lambda)
       ((eq? (car e) (quote cond))
	*cond)
       (else *application)))
     (else *application))))
;Value: list-to-action

(define value
  (lambda (e)
    (meaning e (quote ()))))
;Value: value

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))
;Value: meaning



(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e '#t) #t)
     ((eq? e '#f) #f)
     (else (build (quote primitive) e)))))
;Value: *const



(define *quote
  (lambda (e table)
    (text-of e)))
;Value: *quote

(define text-of second)
;Value: text-of



(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))
;Value: *identifier

(define initial-table
  (lambda (name)
    (car (quote ()))))
;Value: initial-table



(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
	   (cons table (cdr e)))))
;Value: *lambda

(meaning '(lambda (x) (cons x y))
	 '(((y z) ((8) 9))))
;Value 15: (non-primitive ((((y z) ((8) 9))) (x) (cons x y)))

(define table-of first)
;Value: table-of

(define formals-of second)
;Value: formals-of

(define body-of third)
;Value: body-of



(define evcon
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines)))
      (meaning (answer-of (car lines)) table))
     ((meaning (question-of (car lines)) table)
      (meaning (answer-of (car lines)) table))
     (else
      (evcon (cdr lines) table)))))
;Value: evcon

(define else?
  (lambda (x)
    (cond
     ((atom? x) (eq? x (quote else)))
     (else #f))))
;Value: else?

(define question-of first)
;Value: question-of

(define answer-of second)
;Value: answer-of

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))
;Value: *cond

(define cond-lines-of cdr)
;Value: cond-lines-of

;; --------------------------------------------------
;;
;; process generated in evaluating
;;
;; (*cond '(cond (coffee klatsch) (else party))
;;;       '(((coffee) (#t))
;;;	    ((klatsch party) (5 (6)))))

(*cond e table)

(*cond '(cond (coffee klatsch) (else party))
       '(((coffee) (#t))
	 ((klatsch party) (5 (6)))))
;Value: 5

(evcon (cond-lines-of '(cond (coffee klatsch) (else party)))
       '(((coffee) (#t))
	 ((klatsch party) (5 (6)))))
;Value: 5

(evcon '((coffee klatsch) (else party))
       '(((coffee) (#t))
	 ((klatsch party) (5 (6)))))
;Value: 5

(cond
 ((else? (question-of '(coffee klatsch)))
  (meaning (answer-of '(coffee klatsch))
	   '(((coffee) (#t))
	     ((klatsch party) (5 (6))))))
 ((meaning (question-of '(coffee klatsch))
	   '(((coffee) (#t))
	     ((klatsch party) (5 (6)))))
  (meaning (answer-of '(coffee klatsch))
	   '(((coffee) (#t))
	     ((klatsch party) (5 (6))))))
 (else (evcon '((else party))
	      '(((coffee) (#t))
		((klatsch party) (5 (6)))))))
;Value: 5

(cond
 ((else? 'coffee)
  (meaning 'klatsch
	   '(((coffee) (#t))
	     ((klatsch party) (5 (6))))))
 ((meaning 'coffee
	   '(((coffee) (#t))
	     ((klatsch party) (5 (6)))))
  (meaning 'klatsch
	   '(((coffee) (#t))
	     ((klatsch party) (5 (6))))))
 (else (evcon '((else party))
	      '(((coffee) (#t))
		((klatsch party) (5 (6)))))))
;Value: 5

(cond
 ((eq? 'coffee 'else)
  (meaning 'klatsch
	   '(((coffee) (#t))
	     ((klatsch party) (5 (6))))))
 ((meaning 'coffee
	   '(((coffee) (#t))
	     ((klatsch party) (5 (6)))))
  (meaning 'klatsch
	   '(((coffee) (#t))
	     ((klatsch party) (5 (6))))))
 (else (evcon '((else party))
	      '(((coffee) (#t))
		((klatsch party) (5 (6)))))))
;Value: 5

(cond
 ((meaning 'coffee
	   '(((coffee) (#t))
	     ((klatsch party) (5 (6)))))
  (meaning 'klatsch
	   '(((coffee) (#t))
	     ((klatsch party) (5 (6))))))
 (else (evcon '((else party))
	      '(((coffee) (#t))
		((klatsch party) (5 (6)))))))
;Value: 5

(cond
 (((expression-to-action 'coffee)
   'coffee
   '(((coffee) (#t))
     ((klatsch party) (5 (6)))))
  ((expression-to-action 'klatsch)
   'klatsch
   '(((coffee) (#t))
     ((klatsch party) (5 (6))))))
 (else (evcon '((else party))
	      '(((coffee) (#t))
		((klatsch party) (5 (6)))))))
;Value: 5

(cond
 (((cond
    ((atom? 'coffee) (atom-to-action 'coffee))
    (else (list-to-action 'coffee)))
   'coffee
   '(((coffee) (#t))
     ((klatsch party) (5 (6)))))
  ((cond
    ((atom? 'klatsch) (atom-to-action 'klatsch))
    (else (list-to-action 'klatsch)))
   'klatsch
   '(((coffee) (#t))
     ((klatsch party) (5 (6))))))
 (else (evcon '((else party))
	      '(((coffee) (#t))
		((klatsch party) (5 (6)))))))
;Value: 5

(cond
 (((atom-to-action 'coffee)
   'coffee
   '(((coffee) (#t))
     ((klatsch party) (5 (6)))))
  ((atom-to-action 'klatsch)
   'klatsch
   '(((coffee) (#t))
     ((klatsch party) (5 (6))))))
 (else (evcon '((else party))
	      '(((coffee) (#t))
		((klatsch party) (5 (6)))))))
;Value: 5

(cond
 (((cond
    ((number? 'coffee) *const)
    ((eq? 'coffee #t) *const)
    ((eq? 'coffee #f) *const)
    ((eq? 'coffee 'cons) *const)
    ((eq? 'coffee 'car) *const)
    ((eq? 'coffee 'cdr) *const)
    ((eq? 'coffee 'null?) *const)
    ((eq? 'coffee 'eq?) *const)
    ((eq? 'coffee 'atom?) *const)
    ((eq? 'coffee 'zero?) *const)
    ((eq? 'coffee 'add1) *const)
    ((eq? 'coffee 'sub1) *const)
    ((eq? 'coffee 'number?) *const)
    (else *identifier))
   'coffee
   '(((coffee) (#t))
     ((klatsch party) (5 (6)))))
  ((cond
    ((number? 'klatsch) *const)
    ((eq? 'klatsch #t) *const)
    ((eq? 'klatsch #f) *const)
    ((eq? 'klatsch 'cons) *const)
    ((eq? 'klatsch 'car) *const)
    ((eq? 'klatsch 'cdr) *const)
    ((eq? 'klatsch 'null?) *const)
    ((eq? 'klatsch 'eq?) *const)
    ((eq? 'klatsch 'atom?) *const)
    ((eq? 'klatsch 'zero?) *const)
    ((eq? 'klatsch 'add1) *const)
    ((eq? 'klatsch 'sub1) *const)
    ((eq? 'klatsch 'number?) *const)
    (else *identifier))
   'klatsch
   '(((coffee) (#t))
     ((klatsch party) (5 (6))))))
 (else (evcon '((else party))
	      '(((coffee) (#t))
		((klatsch party) (5 (6)))))))
;Value: 5

(cond
 ((*identifier
   'coffee
   '(((coffee) (#t))
     ((klatsch party) (5 (6)))))
  (*identifier
   'klatsch
   '(((coffee) (#t))
     ((klatsch party) (5 (6))))))
 (else (evcon '((else party))
	      '(((coffee) (#t))
		((klatsch party) (5 (6)))))))
;Value: 5

(cond
 ((lookup-in-table 'coffee
		   '(((coffee) (#t))
		     ((klatsch party) (5 (6))))
		   (lambda (name) (car '())))
  (lookup-in-table 'klatsch
		   '(((coffee) (#t))
		     ((klatsch party) (5 (6))))
		   (lambda (name) (car '()))))
 (else (evcon '((else party))
	      '(((coffee) (#t))
		((klatsch party) (5 (6)))))))
;Value: 5

(cond
 ((cond
   ((null? '(((coffee) (#t))
	     ((klatsch party) (5 (6)))))
    ((lambda (name) (car '())) 'coffee))
   (else
    (lookup-in-entry 'coffee
		     (car '(((coffee) (#t))
			    ((klatsch party) (5 (6)))))
		     (lambda (name)
		       (lookup-in-table name
					(cdr '(((coffee) (#t))
					       ((klatsch party) (5 (6)))))
					signal-inexistent)))))
  (cond
   ((null? '(((coffee) (#t))
	     ((klatsch party) (5 (6)))))
    ((lambda (name) (car '())) 'klatsch))
   (else
    (lookup-in-entry 'klatsch
		     (car '(((coffee) (#t))
			    ((klatsch party) (5 (6)))))
		     (lambda (name)
		       (lookup-in-table name
					(cdr '(((coffee) (#t))
					       ((klatsch party) (5 (6)))))
					signal-inexistent))))))
 (else (evcon '((else party))
	      '(((coffee) (#t))
		((klatsch party) (5 (6)))))))
;Value: 5

(cond
 ((lookup-in-entry 'coffee
		   '((coffee) (#t))
		   (lambda (name)
		     (lookup-in-table name
				      (cdr '(((coffee) (#t))
					     ((klatsch party) (5 (6)))))
				      signal-inexistent)))
  (lookup-in-entry 'klatsch
		   '((coffee) (#t))
		   (lambda (name)
		     (lookup-in-table name
				      (cdr '(((coffee) (#t))
					     ((klatsch party) (5 (6)))))
				      signal-inexistent))))
 (else (evcon '((else party))
	      '(((coffee) (#t))
		((klatsch party) (5 (6)))))))
;Value: 5

(cond
 (#t
  ((lambda (name)
     (lookup-in-table name
		      (cdr '(((coffee) (#t))
			     ((klatsch party) (5 (6)))))
		      signal-inexistent))
   'klatsch))
 (else (evcon '((else party))
	      '(((coffee) (#t))
		((klatsch party) (5 (6)))))))
;Value: 5

(cond
 (#t
  (lookup-in-table 'klatsch
		   (cdr '(((coffee) (#t))
			  ((klatsch party) (5 (6)))))
		   (lambda (name)
		     (lookup-in-table name
				      (cdr '(((coffee) (#t))
					     ((klatsch party) (5 (6)))))
				      signal-inexistent)))))
;Value: 5

(cond
 (#t
  (lookup-in-table 'klatsch
		   '(((klatsch party) (5 (6))))
		   (lambda (name)
		     (lookup-in-table name
				      (cdr '(((coffee) (#t))
					     ((klatsch party) (5 (6)))))
				      signal-inexistent)))))
;Value: 5

(cond
 (#t '5))
;Value: 5

5

;; --------------------------------------------------



(define evlis
  (lambda (args table)
    (cond
     ((null? args) (quote ()))
     (else
      (cons (meaning (car args) table)
	    (evlis (cdr args) table))))))
;Value: evlis

(define *application
  (lambda (e table)
    (apply (meaning (function-of e) table)
	   (evlis (arguments-of e) table))))
;Value: *application

(define function-of car)
;Value: function-of

(define arguments-of cdr)
;Value: arguments-of

(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))
;Value: primitive?

(define non-primivite?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))
;Value: non-primivite?

(define apply
  (lambda (fun vals)
    (cond
     ((primitive? fun)
      (apply-primitive (second fun) vals))
     ((non-primitive? fun)
      (apply-closure (second fun) vals)))))
;Value: apply

(define apply-primitive
  (lambda (name vals)
    (cond
     ((eq? name (quote cons))
      (cons (first vals) (second vals)))
     ((eq? name (quote car))
      (car (first vals)))
     ((eq? name (quote cdr))
      (cdr (first vals)))
     ((eq? name (quote null?))
      (null? (first vals)))
     ((eq? name (quote eq?))
      (eq? (first vals) (second vals)))
     ((eq? name (quote atom?))
      (:atom? (first vals)))
     ((eq? name (quote zero?))
      (zero? (first vals)))
     ((eq? name (quote add1))
      (add1 (first vals)))
     ((eq? name (quote sub1))
      (sub1 (first vals)))
     ((eq? name (quote number?))
      (number? (first vals))))))
;Value: apply-primitive

(define :atom?
  (lambda (x)
    (cond
     ((atom? x) #t)
     ((null? x) #f)
     ((eq? (car x) (quote primitive))
      #t)
     ((eq? (car x) (quote non-primitive))
      #t)
     (else #f))))
;Value: :atom?



(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
	     (extend-table (new-entry (formals-of closure)
				      vals)
			   (table-of closure)))))
;Value: apply-closure



;; --------------------------------------------------
;;
;; process generated in evaluating
;;
;; (apply-closure '((((u v w) (1 2 3))
;;;		     ((x y z) (4 5 6)))
;;;		    (x y)
;;;		    (cons z x))
;;;	          '((a b c) (d e f)))

(apply-closure '((((u v w) (1 2 3))
		  ((x y z) (4 5 6)))
		 (x y)
		 (cons z x))
	       '((a b c) (d e f)))
;Value 16: (6 a b c)

(meaning (body-of '((((u v w) (1 2 3))
		     ((x y z) (4 5 6)))
		    (x y)
		    (cons z x)))
	 (extend-table (new-entry (formals-of '((((u v w) (1 2 3))
						 ((x y z) (4 5 6)))
						(x y)
						(cons z x)))
				  '((a b c) (d e f)))
		       (table-of '((((u v w) (1 2 3))
				    ((x y z) (4 5 6)))
				   (x y)
				   (cons z x)))))
;Value 17: (6 a b c)

(meaning '(cons z x)
	 (extend-table '((x y) ((a b c) (d e f)))
		       '(((u v w) (1 2 3))
			 ((x y z) (4 5 6)))))
;Value 18: (6 a b c)

(meaning '(cons z x)
	 '(((x y) ((a b c) (d e f)))
	   ((u v w) (1 2 3))
	   ((x y z) (4 5 6))))
;Value 19: (6 a b c)

((expression-to-action '(cons z x))
 '(cons z x)
 '(((x y) ((a b c) (d e f)))
   ((u v w) (1 2 3))
   ((x y z) (4 5 6))))
;Value 20: (6 a b c)

((list-to-action '(cons z x))
 '(cons z x)
 '(((x y) ((a b c) (d e f)))
   ((u v w) (1 2 3))
   ((x y z) (4 5 6))))
;Value 21: (6 a b c)

(*application
 '(cons z x)
 '(((x y) ((a b c) (d e f)))
   ((u v w) (1 2 3))
   ((x y z) (4 5 6))))
;Value 22: (6 a b c)

(apply (meaning (function-of '(cons z x))
		'(((x y) ((a b c) (d e f)))
		  ((u v w) (1 2 3))
		  ((x y z) (4 5 6))))
       (evlis (arguments-of '(cons z x))
	      '(((x y) ((a b c) (d e f)))
		((u v w) (1 2 3))
		((x y z) (4 5 6)))))
;Value 23: (6 a b c)

(apply (meaning 'cons
		'(((x y) ((a b c) (d e f)))
		  ((u v w) (1 2 3))
		  ((x y z) (4 5 6))))
       (evlis '(z x)
	      '(((x y) ((a b c) (d e f)))
		((u v w) (1 2 3))
		((x y z) (4 5 6)))))
;Value 24: (6 a b c)

(apply ((expression-to-action 'cons)
	'cons
	'(((x y) ((a b c) (d e f)))
	  ((u v w) (1 2 3))
	  ((x y z) (4 5 6))))
       (cons (meaning (car '(z x))
		      '(((x y) ((a b c) (d e f)))
			((u v w) (1 2 3))
			((x y z) (4 5 6))))
	     (evlis (cdr '(z x))
		    '(((x y) ((a b c) (d e f)))
		      ((u v w) (1 2 3))
		      ((x y z) (4 5 6))))))
;Value 25: (6 a b c)

(apply (*const
	'cons
	'(((x y) ((a b c) (d e f)))
	  ((u v w) (1 2 3))
	  ((x y z) (4 5 6))))
       (cons ((expression-to-action 'z)
	      'z
	      '(((x y) ((a b c) (d e f)))
		((u v w) (1 2 3))
		((x y z) (4 5 6))))
	     (evlis '(x)
		    '(((x y) ((a b c) (d e f)))
		      ((u v w) (1 2 3))
		      ((x y z) (4 5 6))))))
;Value 26: (6 a b c)

(apply (build (quote primitive) 'cons)
       (cons (*identifier
	      'z
	      '(((x y) ((a b c) (d e f)))
		((u v w) (1 2 3))
		((x y z) (4 5 6))))
	     (evlis '(x)
		    '(((x y) ((a b c) (d e f)))
		      ((u v w) (1 2 3))
		      ((x y z) (4 5 6))))))
;Value 27: (6 a b c)

(apply '(primitive cons)
       (cons (lookup-in-table 'z
			      '(((x y) ((a b c) (d e f)))
				((u v w) (1 2 3))
				((x y z) (4 5 6)))
			      (lambda (name) (car (quote ()))))
	     (evlis '(x)
		    '(((x y) ((a b c) (d e f)))
		      ((u v w) (1 2 3))
		      ((x y z) (4 5 6))))))
;Value 29: (6 a b c)

(apply '(primitive cons)
       (cons 6
	     (cons (meaning 'x
			    '(((x y) ((a b c) (d e f)))
			      ((u v w) (1 2 3))
			      ((x y z) (4 5 6))))
		   (evlis '()
			  '(((x y) ((a b c) (d e f)))
			    ((u v w) (1 2 3))
			    ((x y z) (4 5 6)))))))
;Value 30: (6 a b c)

(apply '(primitive cons)
       (cons 6
	     (cons ((expression-to-action 'x)
		    'x
		    '(((x y) ((a b c) (d e f)))
		      ((u v w) (1 2 3))
		      ((x y z) (4 5 6))))
		   '())))
;Value 31: (6 a b c)

(apply '(primitive cons)
       (cons 6
	     (cons ((atom-to-action 'x)
		    'x
		    '(((x y) ((a b c) (d e f)))
		      ((u v w) (1 2 3))
		      ((x y z) (4 5 6))))
		   '())))
;Value 32: (6 a b c)

(apply '(primitive cons)
       (cons 6
	     (cons (*identifier
		    'x
		    '(((x y) ((a b c) (d e f)))
		      ((u v w) (1 2 3))
		      ((x y z) (4 5 6))))
		   '())))
;Value 33: (6 a b c)

(apply '(primitive cons)
       (cons 6
	     (cons (lookup-in-table 'x
				    '(((x y) ((a b c) (d e f)))
				      ((u v w) (1 2 3))
				      ((x y z) (4 5 6)))
				    (lambda (name) (car (quote ()))))
		   '())))
;Value 34: (6 a b c)

(apply '(primitive cons)
       (cons 6
	     (cons '(a b c) '())))
;Value 35: (6 a b c)

(apply-primitive (second '(primitive cons))
		 (cons 6
		       (cons '(a b c) '())))
;Value 37: (6 a b c)

(apply-primitive 'cons
		 (cons 6
		       (cons '(a b c) '())))
;Value 38: (6 a b c)

(cons (first (cons 6
		   (cons '(a b c) '())))
      (second (cons 6
		    (cons '(a b c) '()))))
;Value 39: (6 a b c)

(cons (first (cons 6
		   (cons '(a b c) '())))
      (second (cons 6
		    (cons '(a b c) '()))))
;Value 41: (6 a b c)

(cons 6 '(a b c))
;Value 42: (6 a b c)

;; --------------------------------------------------
