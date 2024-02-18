#lang typed/racket

(require racket/set)

(define-type ConsonantFeature
  (U MannerFeature
     LaryngealFeature
     PlaceFeature))

(define-type MannerFeature
  (U 'consonantal
     'sonorant
     'continuant
     'delayed-release
     'approximant
     'tap
     'trill
     'nasal
     'syllabic))

(define-type LaryngealFeature
  (U 'voice
     'spread-glottis
     'constricted-glottis
     'implosive))

(define-type PlaceFeature
  (U 'labial
     'round
     'labiodental
     'coronal
     'anterior
     'distributed
     'strident
     'lateral
     'dorsal
     'high
     'low
     'front
     'back
     'tense))

(define-type VowelFeature
  (U 'high
     'low
     'lax))

(define-type MiscFeature
  (U 'length))

(define-type Feature
  (U ConsonantFeature VowelFeature MiscFeature))

(define-type Phone (Setof Feature))

(provide (all-defined-out))