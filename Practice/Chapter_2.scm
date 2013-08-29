;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;                       Chapter_2.scm
;;                       originated from TLS
;;                       edited by Lawrence R. Amlord(颜世敏 Shi-min Yan)
;;                       informlarry@gmail.com
;;                       Aug 29th, 2013
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

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))
;Value: lat?

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
;Value: atom?

(lat? '(Jack Sprat could eat no chicken fat))
;Value: #t

(lat? '((Jack) Sprat could eat no chicken fat))
;Value: #f

(lat? '(Jack (Sprat could) eat no chicken fat))
;Value: #f

(lat? '())
;Value: #t

(lat? '(bacon and eggs))
;Value: #t

(lat? '(bacon (and eggs)))
;Value: #f

(or (null? '()) (atom? '(d e f g)))
;Value: #t

(or (null? '(a b c)) (null? '()))
;Value: #t

(or (null? '(a c b)) (null? '(atom)))
;Value: #f



(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
	       (member? a (cdr lat)))))))
;Value: member?

(member? 'tea '(coffee tea or milk))
;Value: #t

(member? 'poached '(fried eggs and scrambled eggs))
;Value: #f

(member? 'meat '(mashed potatoes and meat gravy))
;Value: #t

(member? 'liver '(bagel and lox))
;Value: #f

