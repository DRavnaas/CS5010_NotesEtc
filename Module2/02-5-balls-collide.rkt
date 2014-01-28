;; Example: Structural Decomposition on two compound values

;; decompose the first compound, then pass the relevant fields to a
;; help function that decomoses the second compound.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

(define-struct ball (x y radius selected?))

;; A Ball is a (make-ball Number Number Number Boolean)
;; x and y are the coordinates of the center of the ball, in pixels,
;; relative to the origin of the scene.
;; radius is the radius of the ball, in pixels
;; selected? is true iff the ball has been selected for dragging.

;; TEMPLATE:
;; (define (ball-fn b)
;;   (...
;;     (ball-x b)
;;     (ball-y b)
;;     (ball-radius b)
;;     (ball-selected? b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; would-balls-collide? : Ball Ball -> Boolean
;; GIVEN: two balls
;; ANSWERS: do the balls intersect?
;; STRATEGY: Structural Decomposition on b1 : Ball
(define (would-balls-collide? b1 b2)
  (would-ball-collide?
    (ball-x b1) (ball-y b1) (ball-radius b1)
    b2))

;; would-ball-collide? : Number Number Number Ball -> Boolean
;; GIVEN: a set of coordinates and a radius, and a ball b2
;; ANSWERS: Would a ball with the given position and radius collide with b2?
;; STRATEGY: Structural Decomposition on b2 : Ball
(define (would-ball-collide?  x1 y1 r1 b2)
  (would-circles-collide? x1 y1 r1
    (ball-x b2) (ball-y b2) (ball-radius b2)))

;; would-circles-collide? : Number^3 Number^3 -> Boolean
;; GIVEN: two positions and radii
;; ANSWERS: Would two circles with the given positions and radii intersect?
;; STRATEGY: Domain Knowledge
(define (would-circles-collide? x1 y1 r1 x2 y2 r2)
  (<=
    (+
      (square (- x1 x2))
      (square (- y1 y2)))
    (square (+ r1 r2))))
