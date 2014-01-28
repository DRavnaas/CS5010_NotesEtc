;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 03-3-two-draggable-cats) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; two draggable cats.
;; like draggable cat, but there are TWO cats.  They are individually
;; draggable.  But space pauses or unpauses the entire system.

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
;; GIVEN: the initial y-position of the cats
;; EFFECT: runs the simulation, starting with the cats falling
;; RETURNS: the final state of the world
(define (main initial-pos)
  (big-bang (initial-world initial-pos)
            (on-tick world-after-tick 0.5)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define CAT-IMAGE (bitmap "cat.png"))

;; how fast the cat falls, in pixels/tick
(define CATSPEED 8)

;; dimensions of the canvas
(define CANVAS-WIDTH 450)
(define CANVAS-HEIGHT 400)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define CAT1-X-COORD (/ CANVAS-WIDTH 3))
(define CAT2-X-COORD (* 2 CAT1-X-COORD))

;; dimensions of the cat
(define HALF-CAT-WIDTH  (/ (image-width  CAT-IMAGE) 2))
(define HALF-CAT-HEIGHT (/ (image-height CAT-IMAGE) 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS

(define-struct world (cat1 cat2 paused?))
;; A World is a (make-world Cat Cat Boolean)
;; cat1 and cat2 are the two cats
;; paused? describes whether or not the world is paused

;; template:
;; world-fn : World -> ??
;; (define (world-fn w)
;;   (... (world-cat1 w) (world-cat2 w) (world-paused? w)))


(define-struct cat (x-pos y-pos selected?))
;; A Cat is a (make-cat Number Number Boolean)
;; Interpretation: 
;; x-pos, y-pos give the position of the cat. 
;; selected? describes whether or not the cat is selected.

;; template:
;; cat-fn : Cat -> ??
;(define (cat-fn c)
; (... (cat-x-pos w) (cat-y-pos w) (cat-selected? w)))

;; examples of worlds, for testing
(define selected-cat-at-20 (make-cat CAT1-X-COORD 20 true))
(define unselected-cat-at-20 (make-cat CAT1-X-COORD 20 false))

(define selected-cat-at-28 (make-cat CAT1-X-COORD 28 true))
(define unselected-cat-at-28 (make-cat CAT1-X-COORD 28 false))

;; we've now added another dimension of possible inputs.
;; Discussion Board question: Should we now add tests with all these inputs?

;; A FallingCatKeyEvent is a KeyEvent, which is one of
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


;; A FallingCatMouseEvent is a MouseEvent that is one of:
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
;; STRATEGY: structural decomposition on w : World
(define (world-after-tick w)
  (if (world-paused? w)
    w
    (make-world
      (cat-after-tick (world-cat1 w))
      (cat-after-tick (world-cat2 w))
      (world-paused? w))))



;; cat-after-tick : Cat -> Cat
;; GIVEN: a cat c
;; RETURNS: the state of the given cat after a tick if it were in an
;; unpaused world.

;; examples: 
;; cat selected
;; (cat-after-tick selected-cat-at-20) = selected-cat-at-20
;; cat paused:
;; (cat-after-tick unselected-cat-at-20) = unselected-cat-at-28

;; STRATEGY: structural decomposition on c : Cat

(define (cat-after-tick c)
  (cat-after-tick-helper (cat-x-pos c) (cat-y-pos c) (cat-selected? c)))

;; tests: tests follow help function.

;; cat-after-tick-helper : Number Number Boolean -> Cat
;; GIVEN: a position and a value for selected?
;; RETURNS: the cat that should follow one in the given position in an
;; unpaused world 
;; STRATEGY: function composition
(define (cat-after-tick-helper x-pos y-pos selected?)
  (if selected?
    (make-cat x-pos y-pos selected?)
    (make-cat
      x-pos
      (+ y-pos CATSPEED)          
      selected?)))

      

;; tests:
(define-test-suite cat-after-tick-tests

  ;; cat selected
  (check-equal?
    (cat-after-tick selected-cat-at-20)
    selected-cat-at-20
    "selected cat shouldn't move")

  ;; cat unselected
  (check-equal? 
    (cat-after-tick unselected-cat-at-20)
    unselected-cat-at-28
    "unselected cat should fall CATSPEED pixels and remain unselected")

  )

(run-tests cat-after-tick-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE: (world->scene (make-world 20 ??))
;;          = (place-image CAT-IMAGE CAT-X-COORD 20 EMPTY-CANVAS)
;; STRATEGY: structural decomposition w : World
(define (world-to-scene w)
  (place-cat
    (world-cat1 w)
    (place-cat
      (world-cat2 w)
      EMPTY-CANVAS)))

;; place-cat : Cat Scene -> Scene
;; RETURNS: a scene like the given one, but with the given cat painted
;; on it.
(define (place-cat c s)
  (place-image
    CAT-IMAGE
    (cat-x-pos c) (cat-y-pos c)
    s))

;; tests

;; an image showing the cat at Y = 20
;; check this visually to make sure it's what you want
(define image-at-20 (place-image CAT-IMAGE CAT1-X-COORD 20 EMPTY-CANVAS))

;; note: these only test whether world-to-scene calls place-image properly.
;; it doesn't check to see whether that's the right image!
;; these are not very good test strings!
#;(define-test-suite world-to-scene-tests
  (check-equal? 
    (world-to-scene unpaused-world-at-20)
    image-at-20
    "test of (world-to-scene unpaused-world-at-20)")

  (check-equal?
    (world-to-scene paused-world-at-20)
    image-at-20
    "test of (world-to-scene paused-world-at-20)"))

;(run-tests world-to-scene-tests)

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
   (world-cat1 w)
   (world-cat2 w)
   (not (world-paused? w))))


;; for world-after-key-event, we need 4 tests: a paused world, and an
;; unpaused world, and a pause-key-event and a non-pause key event/

#;(define-test-suite world-after-key-event-tests
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

;(run-tests world-after-key-event-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Number Number FallingCatMouseEvent -> World
;; GIVEN: a world and a description of a mouse event
;; RETURNS: the world that should follow the given mouse event
(define (world-after-mouse-event w mx my mev)
  (make-world
    (cat-after-mouse-event (world-cat1 w) mx my mev)
    (cat-after-mouse-event (world-cat2 w) mx my mev)
    (world-paused? w)))



;; cat-after-mouse-event : Cat Number Number FallingCatMouseEvent -> Cat
;; GIVEN: a cat and a description of a mouse event
;; RETURNS: the cat that should follow the given mouse event
;; examples:  See slide on life cycle of dragged cat
;; strategy: struct decomp on mouse events
(define (cat-after-mouse-event c mx my mev)
  (cond
    [(mouse=? mev "button-down") (cat-after-button-down c mx my)]
    [(mouse=? mev "drag") (cat-after-drag c mx my)]
    [(mouse=? mev "button-up")(cat-after-button-up c mx my)]
    [else c]))

;; how many tests do we need here?
;; 3 mouse events (+ a test for the else clause)
;; cat selected or unselected  (do we need to worry about being
;; paused?)
;; event inside cat or not.

#;(define-test-suite world-after-mouse-event-tests

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

;; cat-after-button-down : Cat Number Number -> Cat
;; RETURNS: the cat following a button-down at the given location.
;; STRATEGY: struct decomp on cat
(define (cat-after-button-down c x y)
  (if (in-cat? c x y)
      (make-cat (cat-x-pos c) (cat-y-pos c) true)
      c))

;; cat-after-drag : Cat Number Number -> Cat
;; RETURNS: the cat following a drag at the given location
;; STRATEGY: struct decomp on cat
(define (cat-after-drag c x y)
  (if (cat-selected? c)
      (make-cat x y true)
      c))

;; cat-after-button-up : Cat Number Number -> Cat
;; RETURNS: the cat following a button-up at the given location
;; STRATEGY: struct decomp on cat
(define (cat-after-button-up c x y)
  (if (cat-selected? c)
      (make-cat (cat-x-pos c) (cat-y-pos c) false)
      c))
  

;; in-cat? : Cat Number Number -> Cat
;; RETURNS true iff the given coordinate is inside the bounding box of
;; the given cat.
;; EXAMPLES: see tests below
;; STRATEGY: structural decomposition on c : Cat
(define (in-cat? c x y)
  (and
    (<= 
      (- (cat-x-pos c) HALF-CAT-WIDTH)
      x
      (+ (cat-x-pos c) HALF-CAT-WIDTH))
    (<= 
      (- (cat-y-pos c) HALF-CAT-HEIGHT)
      y
      (+ (cat-y-pos c) HALF-CAT-HEIGHT))))

(define-test-suite in-cat?-tests
  
  ;; inside the cat
  (check-equal?
    (in-cat? unselected-cat-at-20 (+ CAT1-X-COORD 5) 15)
    true
    "test of in-cat? with nearby point")

  (check-equal?
    (in-cat? unselected-cat-at-20 
      (+ CAT1-X-COORD 100) 15)    ;; a coordinate not inside the cat
    false
    "test of in-cat? with distant point")

  )

;; discussion question: are these tests sufficient to test in-cat?

;(run-tests cat-after-mouse-event-tests)
(run-tests in-cat?-tests)

;; initial-world : Number -> World
;; RETURNS: a world with two unselected cats at the given y coordinate
(define (initial-world y)
  (make-world
    (make-cat CAT1-X-COORD y false)
    (make-cat CAT2-X-COORD y false)
    false))



      

