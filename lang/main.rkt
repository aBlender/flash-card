#lang at-exp racket

(provide (struct-out flash-card)
         make-front
         make-back
         view-cards
         make-deck
         view-deck
         test-with-deck)

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
                 (list (entity (position (posn 400 550))
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
  
  (define card-num 0)
  
  (define deck-entity
    (add-or-replace-components
     (apply (curry cutscene #:name (string->symbol deck-name)) all-pages)
     (current-page 0 (begin
                       (set! card-num (index->card-num (get-current-page)))
                       (join (on-key 'enter (^ add1))
                             (on-key 'right (^ (compose (curryr modulo (length all-pages))
                                                        add1)))
                             (on-key 'left (^ (compose (curryr modulo (length all-pages))
                                                       sub1))))))))
  
  (play! #:width 800
         #:height 600
         (game
          (key-manager-entity)
          (delta-time-entity)
          (parent (position (posn 0 0) (go-to-pos-inside 'top-left))
                  (children (entity (sprite (make-text deck-name #:color 'gold)))
                            (bordered-box (+ 20 (* 9 (string-length deck-name))) 28
                                          #:relative-position (posn 0 0)
                                          #:border-color 'white)))
          (parent (position (posn 0 0) (go-to-pos-inside 'top-right))
                  (children (entity (sprite card-num-sprite
                                            (make-text (~a "Card " card-num) #:font-size 24 #:color 'gold)
                                            ))
                            (bordered-box 140 42
                                          #:relative-position (posn 0 0)
                                          #:border-color 'white)))
          deck-entity
          (parent (position (posn 0 0) (go-to-pos-inside 'bottom-center #:offset -40))
                  (children (entity (sprite (make-text "START DECK" #:color 'lightgreen #:font-size 24)))
                            (bordered-box 200 80
                                          #:relative-position (posn 0 0)
                                          )
                            ;(on-sprite-click #:rule (λ (g e) (not (get-entity "Multi Cut Scene" g))) (spawn deck-entity #:relative? #f))
                            ))
          (parent (position (posn 0 0) (go-to-pos 'center))
                  (children (entity (sprite (make-text "THE END")))
                            (bordered-box #:color 'black))))))

(define-component running-page-time number?)

(define (test-with-cards  #:deck-name [deck-name "DECK NAME"] . cards)
  (define (get-front-and-back card)
    (list (add-or-replace-components (flash-card-front card) (duration (flash-card-duration card)))
          (flash-card-back card))) ;should the answer side auto flip?

  (define all-pages
    (flatten (map get-front-and-back cards)))
  
  (define card-num-sprite
    (make-text "Card 1" #:font-size 24 #:color 'gold))
  
  (define card-num 0)

  (define page-time 0)
  
  (define deck-entity
    (add-or-replace-components
     (apply (curry cutscene #:name (string->symbol deck-name)) all-pages)
     (list (current-page 0 (begin
                             (set! card-num (index->card-num (get-current-page)))
                             (join (on-key 'enter (^ add1))
                                   (on-key 'right (^ (compose (curryr modulo (length all-pages))
                                                              add1)))
                                   (on-key 'left (^ (compose (curryr modulo (length all-pages))
                                                             sub1)))
                                   (if (get-duration (list-ref all-pages (get-current-page)))
                                       (on-rule (>= (current-page-time) (get-duration (list-ref all-pages (get-current-page))))
                                                (^ add1))
                                       (get-current-page)))))
           (running-page-time 0 (let ([new-page-time (if (and (< (get-current-page) (length all-pages))
                                                              (get-duration (list-ref all-pages (get-current-page))))
                                                         (- (get-duration (list-ref all-pages (get-current-page)))
                                                            (current-page-time))
                                                         0)])
                                  (set! page-time new-page-time)
                                  new-page-time))
           )))

  
  
  (play! #:width 800
         #:height 600
         (game
          (key-manager-entity)
          (delta-time-entity)
          (parent (position (posn 0 0) (go-to-pos-inside 'top-left))
                  (children (entity (sprite (make-text deck-name #:color 'gold)))
                            (bordered-box (+ 20 (* 9 (string-length deck-name))) 28
                                          #:relative-position (posn 0 0)
                                          #:border-color 'white)))
          (parent (position (posn 0 0) (go-to-pos-inside 'top-right))
                  (children (entity (sprite card-num-sprite
                                            (make-text (~a "Card " card-num) #:font-size 24 #:color 'gold)
                                            ))
                            (bordered-box 140 42
                                          #:relative-position (posn 0 0)
                                          #:border-color 'white)))
          (parent (position (posn 0 0) (go-to-pos-inside 'bottom-center))
                  (children (entity (sprite (make-text "TIME: 0 second(s)"
                                                       #:font-size 16)
                                            (make-text (~a "TIME: " (~r page-time
                                                                        #:precision '(= 3))
                                                           " second(s)")
                                                       #:font-size 16)))
                            (bordered-box 320 24
                                          #:relative-position (posn 0 0))))
          deck-entity
          (parent (position (posn 0 0) (go-to-pos-inside 'bottom-center #:offset -40))
                  (children (entity (sprite (make-text "START DECK" #:color 'lightgreen #:font-size 24)))
                            (bordered-box 200 80
                                          #:relative-position (posn 0 0)
                                          )
                            ;(on-sprite-click #:rule (λ (g e) (not (get-entity "Multi Cut Scene" g))) (spawn deck-entity #:relative? #f))
                            ))
          (parent (position (posn 0 0) (go-to-pos 'center))
                  (children (entity (sprite (make-text "=== RESULTS OVERVIEW ==="
                                                       )))
                            (bordered-box #:color 'black))))))


(define (view-deck deck)
  (apply (curry view-cards #:deck-name (deck-name deck))
         (deck-cards deck)))

(define (test-with-deck deck)
  (apply (curry test-with-cards #:deck-name (deck-name deck))
         (deck-cards deck)))