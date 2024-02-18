#lang typed/racket

(require racket/set)
(require "features.rkt")
(require "vowel.rkt")
(require "consonant.rkt")

(define VOWEL (set 'syllabic
                   'sonorant
                   'continuant
                   'approximant
                   'voice))


;; TODO: check to eliminate contradicting features
(: vowel : (Phone -> Phone) * -> Phone)
(define (vowel . ops)
  (foldr (λ([op : (Phone -> Phone)] [acc : Phone]) (op acc)) VOWEL ops))

(: consonant : (Phone -> Phone) * -> Phone)
(define (consonant . ops)
  (let ([phone (foldr (λ([op : (Phone -> Phone)] [acc : Phone]) (op acc)) (cast (set) Phone) ops)])
    (cond [(and (or (fricative? phone)
                    (affricate? phone))
                (not (lateral? phone))
                (or (alveolar? phone)
                    (palato-alveolar? phone)
                    (retroflex? phone)
                    (alveolopalatal? phone)))
           (set-add phone 'strident)]
          [(glottal? phone)
           (if (fricative? phone)
               (set-add phone 'spread-glottis)
               (set-add phone 'constricted-glottis))]
          [else phone])))
           


;; ============
;;  Predicates
;; ============
(: alveolar? : Phone -> Boolean)
(define (alveolar? phone)
  (place? phone (set 'coronal
                     'anterior)))

(: palato-alveolar? : Phone -> Boolean)
(define (palato-alveolar? phone)
  (place? phone (set 'coronal
                     'distributed)))

(: retroflex? : Phone -> Boolean)
(define (retroflex? phone)
  (place? phone (set 'coronal)))

(: velar? : Phone -> Boolean)
(define (velar? phone)
  (place? phone (set 'dorsal)))

(: uvular? : Phone -> Boolean)
(define (uvular? phone)
  (place? phone (set 'back
                     'dorsal)))

(: alveolopalatal? : Phone -> Boolean)
(define (alveolopalatal? phone)
  (place? phone (alveolopalatal (set))))

(: glottal? : Phone -> Boolean)
(define (glottal? phone)
  (place? phone (set)))

(: fricative? : Phone -> Boolean)
(define (fricative? phone)
  (manner? phone (fricative (set))))

(: affricate? : Phone -> Boolean)
(define (affricate? phone)
  (manner? phone (affricate (set))))

(: lateral? : Phone -> Boolean)
(define (lateral? phone)
  (set-member? phone 'lateral))

;; Does the given phone have all of the given place features,
;; and no place features which aren't given?
(: place? : Phone Phone -> Boolean)
(define (place? phone features)
  (agree? phone (set 'labial
                     'round
                     'labiodental
                     'coronal
                     'anterior
                     'distributed
                     'dorsal
                     'high
                     'low
                     'front
                     'back)))

(: manner? : Phone Phone -> Boolean)
(define (manner? phone features)
  (agree? phone (set 'consonantal
                     'sonorant
                     'continuant
                     'delayed-release
                     'approximant
                     'tap
                     'trill
                     'nasal)))

(: agree? : Phone Phone -> Boolean)
(define (agree? phone features)
  (set=? (set-intersect phone features)
         features))








;; ==================================
;;  Utility Features
;; ==================================

(: minimal-feature-set : (Listof Phone) -> (Setof Feature))
(define (minimal-feature-set phones)
  (let* ([features (feature-set phones)]
         [potential (combinations (set->list features))]
         [potential* (remove-duplicates
                      ((inst map Phone (Listof Feature)) list->set potential)
                      set=?)])
    (foldl (λ([fset : (Setof Feature)] [best-so-far : (Setof Feature)])
             (if (and (or (= 0 (set-count best-so-far))
                          (<= (set-count fset) (set-count best-so-far)))
                      (all-distinct phones fset))
                 (begin (when (<= (set-count fset) 7)
                          (print fset)
                          (println ""))
                        fset)
                 best-so-far))
           (cast (set) Phone)
           potential*)))

(: all-distinct : (Listof Phone) (Setof Feature) -> Boolean)
(define (all-distinct phones fset)
  (not (check-duplicates
        (map (λ([phone : Phone]) (set-intersect phone fset))
             phones)
        set=?)))

(: feature-set : (Listof Phone) -> (Setof Feature))
(define (feature-set phones)
  (if (null? phones)
      (set)
      (apply set-union phones)))


(: natural-class : (Listof Phone) (Listof Phone)
   -> (Listof (Pairof (U '+ '-) (Setof Feature))))
(define (natural-class phones all-phones)
  (let* ([NC (nc phones all-phones)]
         [f+ (first NC)]
         [f- (second NC)])
    (if (nc? phones all-phones f+ f-)
        (list (cons '+ f+)
              (cons '- f-))
        '())))

(: natural-class? : (Listof Phone) (Listof Phone) -> Boolean)
(define (natural-class? phones all-phones)
  (let* ([feats (natural-class phones all-phones)]
         [+feats (assoc '+ feats)]
         [-feats (assoc '- feats)])
    (or (and +feats
             (not (null? (cdr +feats))))
        (and -feats
             (not (null? (cdr -feats)))))))

;; 1. Get union of class features
;; 2. Get union of other features
;; 3. Find set differences
;; 4. If ||{class} / {other}|| > 0, return + features
;; 5. If ||{other} / {class}|| > 0, return - features

(: nc : (Listof Phone) (Listof Phone)
   -> (Listof Phone))
(define (nc class language)
  (let* ([language (set-subtract (list->set language) (list->set class))]
         #;[class (list->set class)]
         [class+ (foldr (λ([p : Phone] [acc : Phone]) (set-intersect p acc)) (first class) class)]
         [classU (foldr (λ([p : Phone] [acc : Phone]) (set-union p acc)) (cast (set) Phone) class)]
         [langU (foldr (λ([p : Phone] [acc : Phone]) (set-union p acc)) (cast (set) Phone) (set->list language))]
         [class- (set-subtract langU class+)]
         [pot+ ((inst map Phone (Listof Feature)) list->set (combinations (set->list class+)))]
         [pot- ((inst map Phone (Listof Feature)) list->set (combinations (set->list class-)))]
         [+/- (cartesian-product pot+ pot-)])
    (foldl (λ([feats : (Listof Phone)] [acc : (Listof Phone)])
             (let ([f+ (first feats)]
                   [f- (second feats)])
               (if (and (< (+ (set-count f+) (set-count f-))
                           (+ (set-count (first acc)) (set-count (second acc))))
                        (nc? class (set->list language) f+ f-))
                   feats
                   acc)))
           (last +/-)
           +/-)))

(: nc? : (Listof Phone) (Listof Phone) Phone Phone -> Boolean)
(define (nc? class language +feats -feats)
  (and (andmap (λ([p : Phone]) (and (set=? +feats (set-intersect p +feats))
                                    (set-empty? (set-intersect p -feats))))
               class)
       (andmap (λ([p : Phone]) (or (not (set=? +feats (set-intersect p +feats)))
                                   (not (set-empty? (set-intersect p -feats)))))
               language)))

(provide (all-defined-out))

