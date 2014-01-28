;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname draggable-cat) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; draggable cat.
;; like falling cat, but user can drag the cat with the mouse.
;; button-down to select, drag to move, button-up to release.

;; falling cat.  
;; A cat falls from the top of the scene.
;; The user can pause/unpause the cat with the space bar.

;; start with (main 0)

(require rackunit)
(require rackunit/text-ui)
(require 2htdp/universe)
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MAIN FUNCTION.

;; main : Number -> World
;; GIVEN: the initial y-position in the cat
;; EFFECT: runs the simulation, starting with the cat falling
;; RETURNS: the final state of the world
(define (main initial-pos)
  (big-bang (make-world CAT-X-COORD initial-pos false false)
            (on-tick world-after-tick 0.5)
            (on-draw world->scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define CAT-IMAGE (bitmap "cat.png"))

;; how fast the cat falls, in pixels/tick
(define CATSPEED 8)

;; dimensions of the canvas
(define CANVAS-WIDTH 200)
(define CANVAS-HEIGHT 400)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define CAT-X-COORD (/ CANVAS-WIDTH 2))

;; dimensions of the cat
(define HALF-CAT-WIDTH  (/ (image-width  CAT-IMAGE) 2))
(define HALF-CAT-HEIGHT (/ (image-height CAT-IMAGE) 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS

(define-struct world (x-pos y-pos paused? selected?))
;; A World is a (make-world Number Number Boolean Boolean)
;; Interpretation: 
;; x-pos, y-pos give the position of the cat. 
;; paused? describes whether or not the cat is paused.
;; selected? describes whether or not the cat is selected.

;; template:
;; world-fn : World -> ??
;(define (world-fn w)
; (... (world-x-pos w) (world-y-pos w) (world-paused? w) (world-selected? w)))

;; examples of worlds, for testing
;; cat is unselected in all of these
(define unpaused-world-at-20 (make-world CAT-X-COORD 20 false false))  
(define paused-world-at-20   (make-world CAT-X-COORD 20 true false))
(define unpaused-world-at-28 (make-world CAT-X-COORD 28 false false))  
(define paused-world-at-28   (make-world CAT-X-COORD 28 true false))
;; examples with cat selected
(define selected-unpaused-world-at-20 (make-world CAT-X-COORD 20 false true))  
(define selected-paused-world-at-20   (make-world CAT-X-COORD 20 true true))
(define selected-unpaused-world-at-28 (make-world CAT-X-COORD 28 false true))  
(define selected-paused-world-at-28   (make-world CAT-X-COORD 28 true
                                        true))

;; we've now added another dimension of possible inputs.
;; Discussion Board question: Should we now add tests with all these inputs?

;; A FallingCatKeyEvent is a partition of KeyEvent into the following
;; categories: 
;; -- " "                (interp: pause/unpause)
;; -- any other KeyEvent (interp: ignore)

;; template:
;; falling-cat-kev-fn : FallingCatKeyEvent -> ??
;(define (falling-cat-kev-fn kev)
;  (cond 
;    [(key=? kev " ") 
;     ...]
;    [else 
;     ...]))

;; examples for testing
(define pause-key-event " ")
(define non-pause-key-event "q")   


;; A FallingCatMouseEvent is a partition of MouseEvent into the
;; following categories:
;; -- "button-down"   (interp: maybe select the cat)
;; -- "drag"          (interp: maybe drag the cat)
;; -- "button-up"     (interp: unselect the cat)
;; -- any other mouse event (interp: ignored)

;(define (mev-fn mev)
;  (cond
;    [(mouse=? mev "button-down") ...]
;    [(mouse=? mev "drag") ...]
;    [(mouse=? mev "button-up") ...]
;    [else ...]))

;;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-tick : World -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow w after a tick.

;; EXAMPLES: 
;; cat falling:
;; (world-after-tick unpaused-world-at-20) = unpaused-world-at-28
;; cat paused:
;; (world-after-tick paused-world-at-20) = paused-world-at-20

;; STRATEGY: structural decomposition on w : World
(define (world-after-tick w)
  (world-after-tick-helper (world-x-pos w) (world-y-pos w)
                           (world-paused? w) (world-selected? w)))

;; tests: tests follow help function.

;; world-after-tick-helper : Number Number Boolean Boolean -> World
;; GIVEN the position of the cat and paused?
;; RETURNS: the next World
(define (world-after-tick-helper x-pos y-pos paused? selected?)
  (if (or paused? selected?)
      (make-world x-pos y-pos paused? selected?)
      (make-world
        x-pos
        (+ y-pos CATSPEED)          
        paused? selected?)))
      

;; tests:
(define-test-suite world-after-tick-tests

  ;; these are for unselected cats:
  (check-equal? 
    (world-after-tick unpaused-world-at-20) 
    unpaused-world-at-28
    "in unpaused world, the cat should fall CATSPEED pixels and world should still be unpaused")

  (check-equal? 
    (world-after-tick paused-world-at-20)
    paused-world-at-20
    "in paused world, cat should be unmoved")

  ;; check to see that selected cats don't move, whether or not they
  ;; are paused
  (check-equal? 
    (world-after-tick selected-unpaused-world-at-20) 
    selected-unpaused-world-at-20
    "selected cat should be unmoved on tick")

  (check-equal? 
    (world-after-tick selected-paused-world-at-20)
    selected-paused-world-at-20
    "selected paused cat should be unmoved on tick")

  )

(run-tests world-after-tick-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world->scene : World -> Scene
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE: (world->scene (make-world 20 ??))
;;          = (place-image CAT-IMAGE CAT-X-COORD 20 EMPTY-CANVAS)
;; STRATEGY: structural decomposition [World]
(define (world->scene w)
  (place-image CAT-IMAGE 
               (world-x-pos w)
               (world-y-pos w)
               EMPTY-CANVAS))

;; tests

;; an image showing the cat at Y = 20
;; check this visually to make sure it's what you want
(define image-at-20 (place-image CAT-IMAGE CAT-X-COORD 20 EMPTY-CANVAS))

;; note: these only test whether world->scene calls place-image properly.
;; it doesn't check to see whether that's the right image!
;; these are not very good test strings!
(define-test-suite world->scene-tests
  (check-equal? 
    (world->scene unpaused-world-at-20)
    image-at-20
    "test of (world->scene unpaused-world-at-20)")

  (check-equal?
    (world->scene paused-world-at-20)
    image-at-20
    "test of (world->scene paused-world-at-20)"))

(run-tests world->scene-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World FallingCatKeyEvent -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow the given world
;; after the given key event.
;; on space, toggle paused?-- ignore all others
;; EXAMPLES: see tests below
;; STRATEGY: structural decomposition on kev : FallingCatKeyEvent
(define (world-after-key-event w kev)
  (cond
    [(key=? kev " ")
     (world-with-paused-toggled w)]
    [else w]))

;; world-with-paused-toggled : World -> World
;; RETURNS: a world just like the given one, but with paused? toggled
;; STRATEGY: structural decomposition on w : World
(define (world-with-paused-toggled w)
  (make-world
   (world-x-pos w)
   (world-y-pos w)
   (not (world-paused? w))
   (world-selected? w)))

;; for world-after-key-event, we need 4 tests: a paused world, and an
;; unpaused world, and a pause-key-event and a non-pause key event/

(define-test-suite world-after-key-event-tests
  (check-equal?
    (world-after-key-event paused-world-at-20 pause-key-event)
    unpaused-world-at-20
    "after pause key, a paused world should become unpaused")

  (check-equal?
    (world-after-key-event unpaused-world-at-20 pause-key-event)
    paused-world-at-20
    "after pause key, an unpaused world should become paused")

  (check-equal?
    (world-after-key-event paused-world-at-20 non-pause-key-event)
    paused-world-at-20
    "after a non-pause key, a paused world should be unchanged")

  (check-equal?
    (world-after-key-event unpaused-world-at-20 non-pause-key-event)
    unpaused-world-at-20
    "after a non-pause key, an unpaused world should be unchanged"))

(run-tests world-after-key-event-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Number Number FallingCatMouseEvent -> World
;; produce the world that should follow the given mouse event
;; examples:  See slide on life cycle of dragged cat
;; strategy: struct decomp on mouse events
(define (world-after-mouse-event w mx my mev)
  (cond
    [(mouse=? mev "button-down") (world-after-button-down w mx my)]
    [(mouse=? mev "drag") (world-after-drag w mx my)]
    [(mouse=? mev "button-up")(world-after-button-up w mx my)]
    [else w]))

;; how many tests do we need here?
;; 3 mouse events (+ a test for the else clause)
;; cat selected or unselected  (do we need to worry about being
;; paused?)
;; event inside cat or not.

(define-test-suite world-after-mouse-event-tests

  ;; for button-down, need tests both inside cat and not
  ;; inside the cat
  (check-equal?
    (world-after-mouse-event unpaused-world-at-20 
      (+ CAT-X-COORD 5) 15    ;; a coordinate inside the cat
      "button-down")
    selected-unpaused-world-at-20
    "button down inside the cat should select it")

  (check-equal?
    (world-after-mouse-event unpaused-world-at-20 
      (+ CAT-X-COORD 100) 15    ;; a coordinate not inside the cat
      "button-down")
    unpaused-world-at-20
    "button down inside the cat should select it")

  ;; do we need tests for button-down on a selected cat?

  ;; tests for drag
  ;; don't care about paused, care only about whether the cat is
  ;; selected.

  (check-equal?
    (world-after-mouse-event unpaused-world-at-20 
      (+ CAT-X-COORD 100) 15    ;; a large motion
      "drag")
    unpaused-world-at-20
    "drag when cat is unselected should leave it unchanged")

  (check-equal?
    (world-after-mouse-event selected-unpaused-world-at-20 
      (+ CAT-X-COORD 100) 15    ;; a large motion
      "drag")
    (make-world 
       (+ CAT-X-COORD 100) 15
       false true)
    "drag when cat is selected should move it to mouse location")

  ;; tests for button-up
  ;; we care only about whether the cat is selected or not.

  ;; possible scenarios:
  ;; button-up on an unselected cat should only happen with the mouse
  ;; not inside the cat.
  ;; the button-up on a selected cat should only happen with the mouse
  ;; inside the cat.

  ;; unselected cat, mouse not in cat
  (check-equal?
    (world-after-mouse-event unpaused-world-at-20 
      (+ CAT-X-COORD 100) 15    ;; a large motion
      "button-up")
    unpaused-world-at-20
    "button-up when cat is unselected should leave it unchanged")

  (check-equal?
    (world-after-mouse-event selected-unpaused-world-at-20 
      (+ CAT-X-COORD 5) 15    ;; a coordinate inside the cat
      "button-up")
    unpaused-world-at-20
    "button-up when cat is selected should make it unselected")

  ;; tests for other mouse events

  (check-equal?
    (world-after-mouse-event unpaused-world-at-20 
      (+ CAT-X-COORD 100) 15    ;; a large motion
      "move")
    unpaused-world-at-20
    "move when cat is unselected should leave it unchanged")

  )

;; can't run these tests until after helper functions are defined

;; world-after-button-down : World Number Number -> World
;; RETURNS: the world following a button-down at the given location.
;; if the button-down is inside the cat, return a cat just like the
;; given one, except that it is selected.
;; STRATEGY: struct decomp on world
(define (world-after-button-down w mx my)
  (if (in-cat? w mx my)
      (make-world (world-x-pos w) (world-y-pos w) 
                  (world-paused? w) true)
      w))

;; world-after-drag : World Number Number -> World
;; RETURNS: the world following a drag at the given location.
;; if the world is selected, then return a world just like the given
;; one, except that it is now centered on the mouse position.
;; STRATEGY: struct decomp on world
(define (world-after-drag w mx my)
  (if (world-selected? w)
      (make-world mx my (world-paused? w) true)
      w))

;; world-after-button-up : World Number Number -> World
;; RETURNS: the world following a button-up at the given location.
;; if the cat is selected, return a cat just like the given one,
;; except that it is no longer selected.
;; STRATEGY: struct decomp on world
(define (world-after-button-up w mx my)
  (if (world-selected? w)
      (make-world (world-x-pos w) (world-y-pos w) 
                  (world-paused? w) false)
      w))
  

;; in-cat? : World Number Number -> World
;; RETURNS: true iff the given coordinate is inside the bounding box of
;; the cat.
;; EXAMPLES: see tests below
;; strategy: structural decomposition on w : World
(define (in-cat? w x y)
  (and
    (<= 
      (- (world-x-pos w) HALF-CAT-WIDTH)
      x
      (+ (world-x-pos w) HALF-CAT-WIDTH))
    (<= 
      (- (world-y-pos w) HALF-CAT-HEIGHT)
      x
      (+ (world-y-pos w) HALF-CAT-HEIGHT))))

(define-test-suite in-cat?-tests
  
  ;; inside the cat
  (check-equal?
    (in-cat? unpaused-world-at-20 (+ CAT-X-COORD 5) 15)
    true
    "test of in-cat? with nearby point")

  (check-equal?
    (in-cat? unpaused-world-at-20 
      (+ CAT-X-COORD 100) 15)    ;; a coordinate not inside the cat
    false
    "test of in-cat? with distant point")

  )

;; discussion question: are these tests sufficient to test in-cat?

(run-tests world-after-mouse-event-tests)
(run-tests in-cat?-tests)


      

