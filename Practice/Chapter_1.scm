;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;                       Chapter_1.scm
;;                       originated from TLS
;;                       edited by Lawrence R. Amlord(颜世敏 Shi-min Yan)
;;                       informlarry@gmail.com
;;                       Aug 28th, 2013
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

(atom? (quote ()))
;Value: #f

(atom? (quote atom))
;Value: #t

(atom? 'atom)
;Value: #t

(atom? 'turkey)
;Value: #t

(atom? '1492)
;Value: #t

(atom? 'u)
;Value: #t

(atom? '*abc$)
;Value: #t

(list? (quote (atom)))
;Value: #t

(list? '(atom))
;Value: #t

(list? '(atom turkey or))
;Value: #t

(list? '(atom turkey) 'or)

;The procedure #[compiled-procedure 14 ("list" #x11) #x1a #x1034b7efa] has been called with 2 arguments; it requires exactly 1 argument.
;To continue, call RESTART with an option number:
; (RESTART 1) => Return to read-eval-print level 1.

(RESTART 1)

;Abort!

(list? '((atom turkey) or))
;Value: #t

(list? '(((how) are) ((you) (doing so)) far))
;Value: #t

(list? '())
;Value: #t

(atom? '())
;Value: #f

(list? '(() () () ()))
;Value: #t

(car '(a b c))
;Value: a

(car '((a b c) x y z))
;Value 15: (a b c)

(car 'hotdog)

;The object hotdog, passed as the first argument to car, is not the correct type.
;To continue, call RESTART with an option number:
; (RESTART 2) => Specify an argument to use in its place.
; (RESTART 1) => Return to read-eval-print level 1.

(RESTART 1)

;Abort!

(car '())

;The object (), passed as the first argument to car, is not the correct type.
;To continue, call RESTART with an option number:
; (RESTART 2) => Specify an argument to use in its place.
; (RESTART 1) => Return to read-eval-print level 1.

(RESTART 1)

;Abort!

(car '(((hotdog)) (and) (pickle) relish))
;Value 16: ((hotdog))

(car (car '(((hotdog)) (and) (pickle) relish)))
;Value 17: (hotdog)

(cdr '(a b c))
;Value 18: (b c)

(cdr '((a b c) x y z))
;Value 19: (x y z)

(cdr '(hamburger))
;Value: ()

(cdr '((x) t r))
;Value 20: (t r)

(cdr 'hotdog)

;The object hotdog, passed as the first argument to cdr, is not the correct type.
;To continue, call RESTART with an option number:
; (RESTART 2) => Specify an argument to use in its place.
; (RESTART 1) => Return to read-eval-print level 1.

(RESTART 1)

;Abort!

(cdr '())

;The object (), passed as the first argument to cdr, is not the correct type.
;To continue, call RESTART with an option number:
; (RESTART 2) => Specify an argument to use in its place.
; (RESTART 1) => Return to read-eval-print level 1.

(RESTART 1)

;Abort!

(car (cdr '((b) (x y) ((c)))))
;Value 21: (x y)

(cdr (cdr '((b) (x y) ((c)))))
;Value 22: (((c)))

(cdr (car '(a (b (c)) d)))

;The object a, passed as the first argument to cdr, is not the correct type.
;To continue, call RESTART with an option number:
; (RESTART 2) => Specify an argument to use in its place.
; (RESTART 1) => Return to read-eval-print level 1.

(RESTART 1)

;Abort!

(cons 'peanut '(butter and jelly))
;Value 23: (peanut butter and jelly)

(cons '(banana and) '(peanut butter and jelly))
;Value 24: ((banana and) peanut butter and jelly)

(cons '((help) this) '(is very ((hard) to learn)))
;Value 25: (((help) this) is very ((hard) to learn))

(cons '(a b (c)) '())
;Value 26: ((a b (c)))

(cons 'a '())
;Value 27: (a)

(cons '((a b c)) 'b)
;Value 28: (((a b c)) . b)

(cons 'a 'b)
;Value 29: (a . b)

(cons 'a (car '((b) c d)))
;Value 31: (a b)

(cons 'a (cdr '((b) c d)))
;Value 32: (a c d)

(null? '())
;Value: #t

(null? (quote ()))
;Value: #t

(null? '(a b c))
;Value: #f

(null? 'spaghetti)
;Value: #f

(atom? 'Harry)
;Value: #t

(atom? '(Harry had a heap of apples))
;Value: #f

(atom? (car '(Harry had a heap of apples)))
;Value: #t

(atom? (cdr '(Harry had a heap of apples)))
;Value: #f

(atom? (cdr '(Harry)))
;Value: #f

(atom? (car (cdr '(swing low sweet cherry oat))))
;Value: #t

(atom? (car (cdr '(swing (low sweet) cherry oat))))
;Value: #f

(eq? 'Harry 'Harry)
;Value: #t

(eq? 'margarine 'butter)
;Value: #f

(eq? '() '(strawberry))
;Value: #f

(eq? 6 7)
;Value: #f

(eq? (car '(Mary had a little lamb chop)) 'Mary)
;Value: #t

(eq? (cdr '(soured milk)) 'milk)
;Value: #f

(define l '(beans beans we need jelly beans))
;Value: l

(eq? (car l) (car (cdr l)))
;Value: #t

