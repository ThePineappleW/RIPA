#lang typed/racket

(require racket/set)
(require "features.rkt")

(: high : Phone -> Phone)
(define (high phone)
  (cast (set-add phone 'high) Phone))

(: low : Phone -> Phone)
(define (low phone)
  (set-add phone 'low))

(: mid : Phone -> Phone)
(define (mid phone)
  phone)

(: tense : Phone -> Phone)
(define (tense phone)
  (set-add phone 'tense))

(: lax : Phone -> Phone)
(define (lax phone)
  phone)

(: round : Phone -> Phone)
(define (round phone)
  (set-add (set-add phone 'labial)
           'round))

(: front : Phone -> Phone)
(define (front phone)
  (cast (set-add phone 'front) Phone))

(: back : Phone -> Phone)
(define (back phone)
  (cast (set-add phone 'back) Phone))

(: nasal* : Phone -> Phone)
(define (nasal* phone)
  (cast (set-add phone 'nasal) Phone))

(: long : Phone -> Phone)
(define (long phone)
  (cast (set-add phone 'length) Phone))

(provide (all-defined-out))