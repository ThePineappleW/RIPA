#lang typed/racket

(require racket/set)
(require "features.rkt")
(require "vowel.rkt")

(: voiced : Phone -> Phone)
(define (voiced phone)
  (set-add phone 'voice))

(: voiceless : Phone -> Phone)
(define (voiceless phone)
  (set-remove phone 'voice))

;; =================
;;  Manner Features
;; =================

(: stop : Phone -> Phone)
(define (stop phone)
  (set-add phone 'consonantal))
(define plosive stop)

(: fricative : Phone -> Phone)
(define (fricative phone)
  (set-union phone (set 'consonantal
                        'continuant
                        'delayed-release)))

(: affricate : Phone -> Phone)
(define (affricate phone)
  (set-union phone (set 'consonantal
                        'delayed-release)))

(: nasal : Phone -> Phone)
(define (nasal phone)
  (set-union phone (set 'consonantal
                        'sonorant
                        'nasal)))

(: trill : Phone -> Phone)
(define (trill phone)
  (set-union phone (set 'consonantal
                        'continuant
                        'sonorant
                        'approximant
                        'trill)))

(: tap : Phone -> Phone)
(define (tap phone)
  (set-union phone (set 'consonantal
                        'continuant
                        'sonorant
                        'approximant
                        'tap)))

(: approximant : Phone -> Phone)
(define (approximant phone)
  (set-union phone (set 'continuant
                        'sonorant
                        'approximant)))

;; Note:
;; Lateral Approximants and Lateral Fricatives
;; Are not given distinct functions;
;; They are instead constructed using `lateral`.


;; ================
;;  Place Features
;; ================

(: bilabial : Phone -> Phone)
(define (bilabial phone)
  (set-add phone 'labial))

(: labiodental : Phone -> Phone)
(define (labiodental phone)
  (set-add (bilabial phone) 'labiodental))

(: coronal : Phone -> Phone)
(define (coronal phone)
  (set-add phone 'coronal))

(: anterior : Phone -> Phone)
(define (anterior phone)
  (set-add phone 'anterior))

(: distributed : Phone -> Phone)
(define (distributed phone)
  (set-add phone 'distributed))

(: dental : Phone -> Phone)
(define (dental phone)
  (coronal (anterior (distributed phone))))

(: alveolar : Phone -> Phone)
(define (alveolar phone)
  (coronal (anterior phone)))

(: palato-alveolar : Phone -> Phone)
(define (palato-alveolar phone)
  (coronal (distributed phone)))

(: retroflex : Phone -> Phone)
(define (retroflex phone)
  (coronal phone))

(: dorsal : Phone -> Phone)
(define (dorsal phone)
  (set-add phone 'dorsal))

(: velar : Phone -> Phone)
(define (velar phone)
  (dorsal phone))

(: uvular : Phone -> Phone)
(define (uvular phone)
  (back (dorsal phone)))

(: pharyngeal : Phone -> Phone)
(define (pharyngeal phone)
  (low (back (dorsal phone))))

(: glottal : Phone -> Phone)
(define (glottal phone)
  phone)

(: lateral : Phone -> Phone)
(define (lateral phone)
  (set-add phone 'lateral))

(: alveolopalatal : Phone -> Phone)
(define (alveolopalatal phone)
  (coronal (anterior (distributed (high (front phone))))))

(: palatal : Phone -> Phone)
(define (palatal phone)
  (coronal (distributed (dorsal (high (front phone))))))

(: labial-velar : Phone -> Phone)
(define (labial-velar phone)
  (bilabial (velar phone)))

;; TODO: add labial-velars and figure out glides

(: aspirated : Phone -> Phone)
(define (aspirated phone)
  (set-add phone 'spread-glottis))

(: implosive : Phone -> Phone)
(define (implosive phone)
  (set-add phone 'implosive))

(provide (all-defined-out))