;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname itemization-then-compound) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Example:

;; Structural Decomposition on partition data, followed by
;; structural decomposition on another value.  The data is handed off
;; to one of several help functions, depending on the value of the
;; partition data. 

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


;; A BallMouseEvent is a partition of MouseEvent into the following
;; categories: 
;; -- "button-down"   interp: select the current ball
;; -- "drag"        interp: drag the current ball
;; -- "button-up" interp: deselect the current ball
;; -- any other MouseEvent  interp: ignored

;; MouseEvent is defined in the 2htdp/universe module. Every MouseEvent is a
;; string, but not every string is a legal mouse event.  The predicate for 
;; comparing mouse events is mouse=? .


;; TEMPLATE:
;; (define (bme-fn mev)
;;   (cond
;;     [(mouse=? mev "button-down") ...]
;;     [(mouse=? mev "drag") ...]
;;     [(mouse=? mev "button-up") ...]
;;     [else ...])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    

;; ball-after-mouse : Ball Number Number MouseEvent -> Ball
;; GIVEN: a ball, a location and a mouse event
;; RETURNS: the ball after the given mouse event at the given location
;; STRATEGY: Structural Decomposition on mev : MouseEvent
(define (ball-after-mouse b mx my mev)
  (cond
    [(mouse=? mev "button-down") (ball-after-button-down b mx my)]
    [(mouse=? mev "drag") (ball-after-drag b mx my)]
    [(mouse=? mev "button-up") (ball-after-button-up b mx my)]
    [else b]))

;; ball-after-drag : Ball Number Number -> Ball
;; GIVEN: a ball and a location
;; RETURNS: the ball after a drag event at the given location.
;; STRATEGY: structural decomposition on b : Ball
(define (ball-after-drag b x y)
  (if (ball-selected? b)
    (ball-moved-to b x y)
    b))

;; ball-moved-to : Ball Number Number -> Ball
;; GIVEN: a ball and a set of coordinates
;; RETURNS: a ball like the given one, except that it has been moved to
;; the given coordinates
;; STRATEGY: structural decomposition on b : Ball
(define (ball-moved-to b x y)
  (make-ball
    x
    y
    (ball-radius b)
    (ball-selected? b)))

;; Wishlist:

;; ball-after-button-down : Ball Number Number -> Ball
;; GIVEN: a ball and a set of coordinates
;; RETURNS: if the given coordinates are inside the ball, returns a ball just
;; like the given one, except that selected? is true.
;; if they are not inside the ball, returns the ball unchanged.
;; STRATEGY: structural decomposition on b : Ball

;; ball-after-button-up : Ball Number Number -> Ball
;; GIVEN: a ball and a set of coordinates
;; RETURNS: a ball just like the given one, except that selected? is
;; false. 
;; STRATEGY: structural decomposition on b : Ball
