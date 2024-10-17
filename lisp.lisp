(quote --welcome-to-lisp--)

(quote --primitives--)
(quote nil)
()
(quote #t)
#t
(quote a-quoted-list)
(quote (a b c))
(quote is-nil-an-atom)
(atom ())
(quote is-#t-an-atom)
(atom #t)
(quote is-quote-abc-an-atom)
(atom (quote (a b c)))
(quote is-nil-eq-nil)
(eq () ())
(quote is-a-eq-a)
(eq (quote a) (quote a))
(quote is-quote-abc-eq-quote-abc)
(eq (quote (a b c)) (quote (a b c)))
(quote car-ab)
(car (quote (a b)))
(quote cdr-ab)
(cdr (quote (a b)))
(quote cons-ab)
(cons (quote a) (cons (quote b) ()))
(quote cond-returns-first-non-nil)
(cond (() (quote 1)) (an-unknown-atom (quote 2)) (#t (quote 3)) (#t (quote 4)))


(quote --logic--)
(define T #t)
(define F ())

(define not (lambda (a) (cond ((eq a F) T))))
(define or (lambda (a b) (cond (a a) (b b))))
(define and (lambda (a b) (not (or (not a) (not b)))))
(define xor (lambda (a b) (and (or a b) (not (and a b)))))

(quote --bits--)

(define 0 F)
(define 1 T)

(define & and)
(define | or)
(define ~ not)
(define ^ xor)

; (a b carry) -> (sum carry)
(define 1bitadd (lambda (a b c)
  (cons
    (^ (^ a b) c)
    (| (& (^ a b) c) (& a b)))))
(quote 1-bit-negate)
(~ 1)
(~ 0)
(quote 1-bit-add)
(1bitadd 0 1 0)
(1bitadd 1 1 0)

; TODO:
; Binary unsigned integer addition + subtraction
; Lisp in Lisp
; C in Lisp
; C in C
