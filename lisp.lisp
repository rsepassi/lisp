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

(quote --helpers--)
(define nil? (lambda (x) (eq x ())))
(nil? ())
(nil? #t)

(define listeq (lambda (a b)
  (cond
    ((and (nil? a) (nil? b)) #t)
    ((nil? a) ())
    ((nil? b) ())
    (#t (and (eq (car a) (car b)) (listeq (cdr a) (cdr b)))))))

(define append (lambda (list1 list2)
  (cond ((nil? list1) list2)
        (#t (cons (car list1) (append (cdr list1) list2))))))
(append (quote (1 2 3)) (quote (4 5 6)))

(define reverse-helper (lambda (a acc)
  (cond ((eq () a) acc)
        (#t (reverse-helper (cdr a) (cons (car a) acc))))))
(define reverse (lambda (a)
  (reverse-helper a ())))
(reverse (quote (1 2 3 4)))

(quote --logic--)
(define T #t)
(define F ())

; We build our logic out of not (not-nil) and or (cond)
(define not (lambda (a) (cond ((nil? a) T))))
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

(quote 1-bit-negate)
(~ 1)
(~ 0)

; (a b carry) -> (sum carry)
(define 1bitadd (lambda (a b c)
  (cons
    (^ (^ a b) c)
    (| (& (^ a b) c) (& a b)))))
(1bitadd 0 1 0)
(1bitadd 1 1 0)

(define 0x00 (quote (() () () () () () () ())))
(define 0x01 (quote (#t () () () () () () ())))
(define 0x02 (quote (() #t () () () () () ())))
(define 0x03 (quote (#t #t () () () () () ())))
(define 0x04 (quote (() () #t () () () () ())))
(define 0x05 (quote (#t () #t () () () () ())))
(define 0x06 (quote (() #t #t () () () () ())))
(define 0x07 (quote (#t #t #t () () () () ())))
(define 0x08 (quote (() () () #t () () () ())))
(define 0x09 (quote (#t () () #t () () () ())))
(define 0x0a (quote (() #t () #t () () () ())))
(define 0x0b (quote (#t #t () #t () () () ())))
(define 0x0c (quote (() () #t #t () () () ())))
(define 0x0d (quote (#t () #t #t () () () ())))
(define 0x0e (quote (() #t #t #t () () () ())))
(define 0x0f (quote (#t #t #t #t () () () ())))
(define 0x10 (quote (() () () () #t () () ())))
(define 0x11 (quote (#t () () () #t () () ())))
(define 0x12 (quote (() #t () () #t () () ())))
(define 0x13 (quote (#t #t () () #t () () ())))
(define 0x14 (quote (() () #t () #t () () ())))
(define 0x15 (quote (#t () #t () #t () () ())))
(define 0x16 (quote (() #t #t () #t () () ())))
(define 0x17 (quote (#t #t #t () #t () () ())))
(define 0x18 (quote (() () () #t #t () () ())))
(define 0x19 (quote (#t () () #t #t () () ())))
(define 0x1a (quote (() #t () #t #t () () ())))
(define 0x1b (quote (#t #t () #t #t () () ())))
(define 0x1c (quote (() () #t #t #t () () ())))
(define 0x1d (quote (#t () #t #t #t () () ())))
(define 0x1e (quote (() #t #t #t #t () () ())))
(define 0x1f (quote (#t #t #t #t #t () () ())))
(define 0x20 (quote (() () () () () #t () ())))
(define 0x21 (quote (#t () () () () #t () ())))
(define 0x22 (quote (() #t () () () #t () ())))
(define 0x23 (quote (#t #t () () () #t () ())))
(define 0x24 (quote (() () #t () () #t () ())))
(define 0x25 (quote (#t () #t () () #t () ())))
(define 0x26 (quote (() #t #t () () #t () ())))
(define 0x27 (quote (#t #t #t () () #t () ())))
(define 0x28 (quote (() () () #t () #t () ())))
(define 0x29 (quote (#t () () #t () #t () ())))
(define 0x2a (quote (() #t () #t () #t () ())))
(define 0x2b (quote (#t #t () #t () #t () ())))
(define 0x2c (quote (() () #t #t () #t () ())))
(define 0x2d (quote (#t () #t #t () #t () ())))
(define 0x2e (quote (() #t #t #t () #t () ())))
(define 0x2f (quote (#t #t #t #t () #t () ())))
(define 0x30 (quote (() () () () #t #t () ())))
(define 0x31 (quote (#t () () () #t #t () ())))
(define 0x32 (quote (() #t () () #t #t () ())))
(define 0x33 (quote (#t #t () () #t #t () ())))
(define 0x34 (quote (() () #t () #t #t () ())))
(define 0x35 (quote (#t () #t () #t #t () ())))
(define 0x36 (quote (() #t #t () #t #t () ())))
(define 0x37 (quote (#t #t #t () #t #t () ())))
(define 0x38 (quote (() () () #t #t #t () ())))
(define 0x39 (quote (#t () () #t #t #t () ())))
(define 0x3a (quote (() #t () #t #t #t () ())))
(define 0x3b (quote (#t #t () #t #t #t () ())))
(define 0x3c (quote (() () #t #t #t #t () ())))
(define 0x3d (quote (#t () #t #t #t #t () ())))
(define 0x3e (quote (() #t #t #t #t #t () ())))
(define 0x3f (quote (#t #t #t #t #t #t () ())))
(define 0x40 (quote (() () () () () () #t ())))
(define 0x41 (quote (#t () () () () () #t ())))
(define 0x42 (quote (() #t () () () () #t ())))
(define 0x43 (quote (#t #t () () () () #t ())))
(define 0x44 (quote (() () #t () () () #t ())))
(define 0x45 (quote (#t () #t () () () #t ())))
(define 0x46 (quote (() #t #t () () () #t ())))
(define 0x47 (quote (#t #t #t () () () #t ())))
(define 0x48 (quote (() () () #t () () #t ())))
(define 0x49 (quote (#t () () #t () () #t ())))
(define 0x4a (quote (() #t () #t () () #t ())))
(define 0x4b (quote (#t #t () #t () () #t ())))
(define 0x4c (quote (() () #t #t () () #t ())))
(define 0x4d (quote (#t () #t #t () () #t ())))
(define 0x4e (quote (() #t #t #t () () #t ())))
(define 0x4f (quote (#t #t #t #t () () #t ())))
(define 0x50 (quote (() () () () #t () #t ())))
(define 0x51 (quote (#t () () () #t () #t ())))
(define 0x52 (quote (() #t () () #t () #t ())))
(define 0x53 (quote (#t #t () () #t () #t ())))
(define 0x54 (quote (() () #t () #t () #t ())))
(define 0x55 (quote (#t () #t () #t () #t ())))
(define 0x56 (quote (() #t #t () #t () #t ())))
(define 0x57 (quote (#t #t #t () #t () #t ())))
(define 0x58 (quote (() () () #t #t () #t ())))
(define 0x59 (quote (#t () () #t #t () #t ())))
(define 0x5a (quote (() #t () #t #t () #t ())))
(define 0x5b (quote (#t #t () #t #t () #t ())))
(define 0x5c (quote (() () #t #t #t () #t ())))
(define 0x5d (quote (#t () #t #t #t () #t ())))
(define 0x5e (quote (() #t #t #t #t () #t ())))
(define 0x5f (quote (#t #t #t #t #t () #t ())))
(define 0x60 (quote (() () () () () #t #t ())))
(define 0x61 (quote (#t () () () () #t #t ())))
(define 0x62 (quote (() #t () () () #t #t ())))
(define 0x63 (quote (#t #t () () () #t #t ())))
(define 0x64 (quote (() () #t () () #t #t ())))
(define 0x65 (quote (#t () #t () () #t #t ())))
(define 0x66 (quote (() #t #t () () #t #t ())))
(define 0x67 (quote (#t #t #t () () #t #t ())))
(define 0x68 (quote (() () () #t () #t #t ())))
(define 0x69 (quote (#t () () #t () #t #t ())))
(define 0x6a (quote (() #t () #t () #t #t ())))
(define 0x6b (quote (#t #t () #t () #t #t ())))
(define 0x6c (quote (() () #t #t () #t #t ())))
(define 0x6d (quote (#t () #t #t () #t #t ())))
(define 0x6e (quote (() #t #t #t () #t #t ())))
(define 0x6f (quote (#t #t #t #t () #t #t ())))
(define 0x70 (quote (() () () () #t #t #t ())))
(define 0x71 (quote (#t () () () #t #t #t ())))
(define 0x72 (quote (() #t () () #t #t #t ())))
(define 0x73 (quote (#t #t () () #t #t #t ())))
(define 0x74 (quote (() () #t () #t #t #t ())))
(define 0x75 (quote (#t () #t () #t #t #t ())))
(define 0x76 (quote (() #t #t () #t #t #t ())))
(define 0x77 (quote (#t #t #t () #t #t #t ())))
(define 0x78 (quote (() () () #t #t #t #t ())))
(define 0x79 (quote (#t () () #t #t #t #t ())))
(define 0x7a (quote (() #t () #t #t #t #t ())))
(define 0x7b (quote (#t #t () #t #t #t #t ())))
(define 0x7c (quote (() () #t #t #t #t #t ())))
(define 0x7d (quote (#t () #t #t #t #t #t ())))
(define 0x7e (quote (() #t #t #t #t #t #t ())))
(define 0x7f (quote (#t #t #t #t #t #t #t ())))
(define 0x80 (quote (() () () () () () () #t)))
(define 0x81 (quote (#t () () () () () () #t)))
(define 0x82 (quote (() #t () () () () () #t)))
(define 0x83 (quote (#t #t () () () () () #t)))
(define 0x84 (quote (() () #t () () () () #t)))
(define 0x85 (quote (#t () #t () () () () #t)))
(define 0x86 (quote (() #t #t () () () () #t)))
(define 0x87 (quote (#t #t #t () () () () #t)))
(define 0x88 (quote (() () () #t () () () #t)))
(define 0x89 (quote (#t () () #t () () () #t)))
(define 0x8a (quote (() #t () #t () () () #t)))
(define 0x8b (quote (#t #t () #t () () () #t)))
(define 0x8c (quote (() () #t #t () () () #t)))
(define 0x8d (quote (#t () #t #t () () () #t)))
(define 0x8e (quote (() #t #t #t () () () #t)))
(define 0x8f (quote (#t #t #t #t () () () #t)))
(define 0x90 (quote (() () () () #t () () #t)))
(define 0x91 (quote (#t () () () #t () () #t)))
(define 0x92 (quote (() #t () () #t () () #t)))
(define 0x93 (quote (#t #t () () #t () () #t)))
(define 0x94 (quote (() () #t () #t () () #t)))
(define 0x95 (quote (#t () #t () #t () () #t)))
(define 0x96 (quote (() #t #t () #t () () #t)))
(define 0x97 (quote (#t #t #t () #t () () #t)))
(define 0x98 (quote (() () () #t #t () () #t)))
(define 0x99 (quote (#t () () #t #t () () #t)))
(define 0x9a (quote (() #t () #t #t () () #t)))
(define 0x9b (quote (#t #t () #t #t () () #t)))
(define 0x9c (quote (() () #t #t #t () () #t)))
(define 0x9d (quote (#t () #t #t #t () () #t)))
(define 0x9e (quote (() #t #t #t #t () () #t)))
(define 0x9f (quote (#t #t #t #t #t () () #t)))
(define 0xa0 (quote (() () () () () #t () #t)))
(define 0xa1 (quote (#t () () () () #t () #t)))
(define 0xa2 (quote (() #t () () () #t () #t)))
(define 0xa3 (quote (#t #t () () () #t () #t)))
(define 0xa4 (quote (() () #t () () #t () #t)))
(define 0xa5 (quote (#t () #t () () #t () #t)))
(define 0xa6 (quote (() #t #t () () #t () #t)))
(define 0xa7 (quote (#t #t #t () () #t () #t)))
(define 0xa8 (quote (() () () #t () #t () #t)))
(define 0xa9 (quote (#t () () #t () #t () #t)))
(define 0xaa (quote (() #t () #t () #t () #t)))
(define 0xab (quote (#t #t () #t () #t () #t)))
(define 0xac (quote (() () #t #t () #t () #t)))
(define 0xad (quote (#t () #t #t () #t () #t)))
(define 0xae (quote (() #t #t #t () #t () #t)))
(define 0xaf (quote (#t #t #t #t () #t () #t)))
(define 0xb0 (quote (() () () () #t #t () #t)))
(define 0xb1 (quote (#t () () () #t #t () #t)))
(define 0xb2 (quote (() #t () () #t #t () #t)))
(define 0xb3 (quote (#t #t () () #t #t () #t)))
(define 0xb4 (quote (() () #t () #t #t () #t)))
(define 0xb5 (quote (#t () #t () #t #t () #t)))
(define 0xb6 (quote (() #t #t () #t #t () #t)))
(define 0xb7 (quote (#t #t #t () #t #t () #t)))
(define 0xb8 (quote (() () () #t #t #t () #t)))
(define 0xb9 (quote (#t () () #t #t #t () #t)))
(define 0xba (quote (() #t () #t #t #t () #t)))
(define 0xbb (quote (#t #t () #t #t #t () #t)))
(define 0xbc (quote (() () #t #t #t #t () #t)))
(define 0xbd (quote (#t () #t #t #t #t () #t)))
(define 0xbe (quote (() #t #t #t #t #t () #t)))
(define 0xbf (quote (#t #t #t #t #t #t () #t)))
(define 0xc0 (quote (() () () () () () #t #t)))
(define 0xc1 (quote (#t () () () () () #t #t)))
(define 0xc2 (quote (() #t () () () () #t #t)))
(define 0xc3 (quote (#t #t () () () () #t #t)))
(define 0xc4 (quote (() () #t () () () #t #t)))
(define 0xc5 (quote (#t () #t () () () #t #t)))
(define 0xc6 (quote (() #t #t () () () #t #t)))
(define 0xc7 (quote (#t #t #t () () () #t #t)))
(define 0xc8 (quote (() () () #t () () #t #t)))
(define 0xc9 (quote (#t () () #t () () #t #t)))
(define 0xca (quote (() #t () #t () () #t #t)))
(define 0xcb (quote (#t #t () #t () () #t #t)))
(define 0xcc (quote (() () #t #t () () #t #t)))
(define 0xcd (quote (#t () #t #t () () #t #t)))
(define 0xce (quote (() #t #t #t () () #t #t)))
(define 0xcf (quote (#t #t #t #t () () #t #t)))
(define 0xd0 (quote (() () () () #t () #t #t)))
(define 0xd1 (quote (#t () () () #t () #t #t)))
(define 0xd2 (quote (() #t () () #t () #t #t)))
(define 0xd3 (quote (#t #t () () #t () #t #t)))
(define 0xd4 (quote (() () #t () #t () #t #t)))
(define 0xd5 (quote (#t () #t () #t () #t #t)))
(define 0xd6 (quote (() #t #t () #t () #t #t)))
(define 0xd7 (quote (#t #t #t () #t () #t #t)))
(define 0xd8 (quote (() () () #t #t () #t #t)))
(define 0xd9 (quote (#t () () #t #t () #t #t)))
(define 0xda (quote (() #t () #t #t () #t #t)))
(define 0xdb (quote (#t #t () #t #t () #t #t)))
(define 0xdc (quote (() () #t #t #t () #t #t)))
(define 0xdd (quote (#t () #t #t #t () #t #t)))
(define 0xde (quote (() #t #t #t #t () #t #t)))
(define 0xdf (quote (#t #t #t #t #t () #t #t)))
(define 0xe0 (quote (() () () () () #t #t #t)))
(define 0xe1 (quote (#t () () () () #t #t #t)))
(define 0xe2 (quote (() #t () () () #t #t #t)))
(define 0xe3 (quote (#t #t () () () #t #t #t)))
(define 0xe4 (quote (() () #t () () #t #t #t)))
(define 0xe5 (quote (#t () #t () () #t #t #t)))
(define 0xe6 (quote (() #t #t () () #t #t #t)))
(define 0xe7 (quote (#t #t #t () () #t #t #t)))
(define 0xe8 (quote (() () () #t () #t #t #t)))
(define 0xe9 (quote (#t () () #t () #t #t #t)))
(define 0xea (quote (() #t () #t () #t #t #t)))
(define 0xeb (quote (#t #t () #t () #t #t #t)))
(define 0xec (quote (() () #t #t () #t #t #t)))
(define 0xed (quote (#t () #t #t () #t #t #t)))
(define 0xee (quote (() #t #t #t () #t #t #t)))
(define 0xef (quote (#t #t #t #t () #t #t #t)))
(define 0xf0 (quote (() () () () #t #t #t #t)))
(define 0xf1 (quote (#t () () () #t #t #t #t)))
(define 0xf2 (quote (() #t () () #t #t #t #t)))
(define 0xf3 (quote (#t #t () () #t #t #t #t)))
(define 0xf4 (quote (() () #t () #t #t #t #t)))
(define 0xf5 (quote (#t () #t () #t #t #t #t)))
(define 0xf6 (quote (() #t #t () #t #t #t #t)))
(define 0xf7 (quote (#t #t #t () #t #t #t #t)))
(define 0xf8 (quote (() () () #t #t #t #t #t)))
(define 0xf9 (quote (#t () () #t #t #t #t #t)))
(define 0xfa (quote (() #t () #t #t #t #t #t)))
(define 0xfb (quote (#t #t () #t #t #t #t #t)))
(define 0xfc (quote (() () #t #t #t #t #t #t)))
(define 0xfd (quote (#t () #t #t #t #t #t #t)))
(define 0xfe (quote (() #t #t #t #t #t #t #t)))
(define 0xff (quote (#t #t #t #t #t #t #t #t)))

; n-bit add, little-endian (i.e. low-bits are first)
(define bitadd-helper (lambda (c a b)
  (cond
    ; if a/b is nil, add a/b and c
    ((nil? a)
     (cond
       ((nil? c) b)
       (#t (bitadd 0 (cons c ()) b))))
    ((nil? b)
     (cond
       ((nil? c) a)
       (#t (bitadd 0 (cons c ()) a))))
    (#t
    ; otherwise, we add the first bits
    ; and recurse on the remainder
    ((lambda (first-add)
       (cons (car first-add) (bitadd-helper (cdr first-add) (cdr a) (cdr b)))
     ) (1bitadd (car a) (car b) c))
    )
  )))
(define bitadd (lambda (a b)
  (bitadd-helper 0 a b)))

(listeq (bitadd 0xff 0xe3) (quote (() #t () () () #t #t #t #t)))
