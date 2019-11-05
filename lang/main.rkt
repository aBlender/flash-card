#lang at-exp racket

(provide (struct-out flash-card)
         make-front
         make-back
         view-cards
         make-deck
         view-deck)

(require meta-engine
         2htdp/image
         racket/runtime-path
         threading
         (only-in pict
                  filled-rounded-rectangle
                  inset
                  pict->bitmap)
         image-coloring)

; ==== FLASH CARD MVP PROTOTYPE ====
; Deck Name
; Front/Back styles
; Supports Text, Images, and Sprites
; Todo: Add Code Snippet style
;       Arrow Key Navigation
;       Mouse Based Navigation
;       Multiple Choice and Assessments
;       Re-base on top of meta-engine

(struct flash-card (front back duration))
(struct deck (name cards))

(define (make-deck name . cards)
  (deck name cards))

(define (make-front . things)
  (define (maybe-increase-font thing)
    (if (string? thing)
        (make-text thing #:font-size 24)
        thing))
  (apply (curry page
                ;#:name "Front"
                #:bg-color (color 50 50 50)
                #:border-color 'lightgreen)
         (append (list (entity (position (posn 400 40))
                               (sprite (make-text "FRONT"
                                                  #:color 'lightgreen
                                                  #:font-size 20))))
                 (map maybe-increase-font things)
                 )))

(define (make-back . things)
  (define (maybe-increase-font thing)
    (if (string? thing)
        (make-text thing #:font-size 24)
        thing))
  (apply (curry page
                ;#:name "Back"
                #:bg-color (color 80 80 80)
                #:border-color 'lightblue)
         (append (list (entity (position (posn 400 40))
                               (sprite (make-text "BACK"
                                                  #:color 'lightblue
                                                  #:font-size 20))))
                 (map maybe-increase-font things)
                 (list (entity (position (posn 400 560))
                               (sprite (make-text "Press [enter] to continue..."
                                                  #:color 'lightblue
                                                  #:font-size 16))))
                 )))

(define (index->card-num index)
  (add1 (exact-floor (/ index 2))))

(define (remove-death e)
    (remove-component e (λ(c) (eq? (get-component-name c) 'death))))

(define (view-cards  #:deck-name [deck-name "DECK NAME"] . cards)
  (define (get-front-and-back card)
    (list (flash-card-front card)
          (flash-card-back card)))
  
  (define all-pages
    (flatten (map get-front-and-back cards)))
  
  (define card-num-sprite
    (make-text "Card 1" #:font-size 24 #:color 'gold))
  
  ;(define (update-card-num g e)
  ;  (define index (get-counter (get-entity "Multi Cut Scene" g)))
  ;  (update-entity e (curry component-eq? card-num-sprite) (set-frames (~a "Card " (index->card-num index)) card-num-sprite)))

  (define deck-entity
    (apply (curry cutscene #:name deck-name) all-pages)
    #|(~> (apply cutscene all-pages)
        (update-entity _ layer? (layer "tops"))
        (add-components _
                        (on-key 'right (change-cutscene-by 1))
                        (on-key 'left (change-cutscene-by -1))))|#

    )
  
  (play! #:width 800
         #:height 600
         (game
          (key-manager-entity)
          (delta-time-entity)
          (parent (position (posn 90 20))
                  (children (entity (sprite (make-text deck-name #:color 'gold)))
                            (bordered-box (+ 20 (* 9 (string-length deck-name))) 28
                                          #:relative-position (posn 0 0)
                                          #:border-color 'white)))
          (parent (position (posn 720 24)) ;(go-to-pos-inside 'top-right)
                  (children (entity (sprite card-num-sprite
                                            ;(get-counter (get-entity (CURRENT-GAME)
                                            ;                         (has-name deck-name)))
                                            ))
                            (bordered-box 124 42
                                          #:relative-position (posn 0 0)
                                          #:border-color 'white)))
          deck-entity
          (parent (position (posn 400 580)) ;(go-to-pos-inside 'bottom-center #:offset -40)
                  (children (entity (sprite (make-text "START DECK" #:color 'lightgreen #:font-size 24)))
                            (bordered-box 200 80)
                 ;(on-sprite-click #:rule (λ (g e) (not (get-entity "Multi Cut Scene" g))) (spawn deck-entity #:relative? #f))
                            ))
          (parent (position (posn 0 0) (go-to-center))
                  (children (entity (sprite (make-text "THE END")))
                            (bordered-box))))))

#|(define (test-with-cards  #:deck-name [deck-name "DECK NAME"] . cards)
  (define (get-front-and-back card)
    (list (set-storage "duration" (flash-card-front card) (flash-card-duration card))
          (flash-card-back card))) ;should the answer side auto flip?
  
  (define all-pages
    (flatten (map get-front-and-back cards)))

  (define card-num-sprite
    (make-text "Card 1" #:font-size 24 #:color 'gold))
  
  (define (update-card-num g e)
    (define index (get-counter (get-entity "Multi Cut Scene" g)))
    (update-entity e (curry component-eq? card-num-sprite) (set-frames (~a "Card " (index->card-num index)) card-num-sprite)))

  (define deck-entity
    (~> (apply cutscene all-pages)
        (update-entity _ layer? (layer "tops"))
        (add-components _
                        (on-key 'right (change-cutscene-by 1))
                        (on-key 'left (change-cutscene-by -1)))))
  
  (start-game (sprite->entity (list (make-text deck-name #:color 'gold)
                                    (bordered-box-sprite (+ 20 (* 9 (string-length deck-name))) 28
                                                         #:border-color 'white))
                              #:name "deck-name"
                              #:position (posn 0 0)
                              #:components (layer "ui")
                                           (on-start (go-to-pos-inside 'top-left)))
              (sprite->entity (list card-num-sprite
                                    (bordered-box-sprite 124 42 #:border-color 'white))
                              #:name "card-number"
                              #:position (posn 0 0)
                              #:components (layer "ui")
                                           (on-start (go-to-pos-inside 'top-right))
                                           (observe-change (λ (g e)
                                                             (and (get-entity "Multi Cut Scene" g)
                                                                  (get-counter (get-entity "Multi Cut Scene" g))))
                                                           (λ (g e1 e2)
                                                             (if (get-entity "Multi Cut Scene" g)
                                                                 (update-card-num g e2)
                                                                 e2))))
              deck-entity
              (sprite->entity (list (make-text "START DECK" #:color 'lightgreen #:font-size 24)
                                    (bordered-box-sprite 200 80 #:border-color 'lightgreen))
                              #:name "start-button"
                              #:position   (posn 0 0)
                              #:components (on-start (go-to-pos-inside 'bottom-center #:offset -40))
                                           (on-sprite-click #:rule (λ (g e) (not (get-entity "Multi Cut Scene" g))) (spawn deck-entity #:relative? #f)))
              (sprite->entity (list (make-text "THE END")
                                    (bordered-box-sprite 800 600 #:color 'black))
                              #:name "bg"
                              #:position (posn 0 0))))
|#

(define (view-deck deck)
  (apply (curry view-cards #:deck-name (deck-name deck))
         (deck-cards deck)))

#;(define (test-with-deck deck)
  (apply (curry test-with-cards #:deck-name (deck-name deck))
         (deck-cards deck)))