#lang racket

(require "phones.rkt")
(require "vowel.rkt")
(require "consonant.rkt")

(define i (vowel high front tense))
(define u (vowel high back tense))
(define short-vowels
  (list i                        ; i
        u                        ; u
        (vowel front lax)        ; E
        (vowel back tense)       ; o
        (vowel back lax)         ; O
        (vowel low)))            ; a

(define all-vowels (foldr (λ(x y) (cons (long x) y))
                          short-vowels
                          short-vowels))

(define c consonant)
(define vl voiceless)
(define asp aspirated)
(define Nasal (compose voiced nasal))

(define p (c vl bilabial stop))
(define ph (c asp vl bilabial stop))
(define t (c vl alveolar stop))
(define th (c asp vl alveolar stop))
(define C (c vl palatal stop))
(define ch (c vl asp palatal stop))
(define k (c vl velar stop))
(define kh (c asp vl velar stop))
(define ? (c glottal stop))

(define b (c implosive voiced bilabial stop))
(define d (c implosive voiced alveolar stop))

(define s (c vl alveolar fricative))
(define x (c vl velar fricative))
(define h (c vl glottal fricative))

(define m (c bilabial Nasal))
(define n (c alveolar Nasal))
(define ny (c palatal Nasal))
(define ng (c velar Nasal))

(define l (c voiced alveolar lateral fricative))
(define j (c voiced palatal approximant))
(define w (c voiced labial-velar approximant))


(define lamet
  (list p  t  C  k   ?
        ph th ch kh
        b  d
           s     x   h
        m  n  ny ng
     s      l
              j  w))

(define min-set (set
 'dorsal
 'sonorant
 'spread-glottis
 'coronal
 'continuant
 'voice
 'labial))

(define (min-feats phones)
  (map (λ(p) (set-intersect p min-set)) phones))



;;(minimal-feature-set all-vowels)