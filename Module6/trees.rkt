;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; trees.rkt

(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require rackunit/text-ui)
;(require "extras.rkt")

;; run with (run 0)

;(provide run)
;(provide initial-world)
;(provide world-after-key-event)
;(provide world-after-mouse-event)
;(provide world-to-roots)
;(provide node-to-center)
;(provide node-to-sons)
;(provide node-to-selected?)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS
(define SQUARE-SIDE 20)

;; dimensions of the canvas
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 400)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

(define CANVAS-CENTER-X (/ CANVAS-WIDTH 2))
(define CANVAS-CENTER-Y (/ CANVAS-HEIGHT 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS

;; A MyMouseEvent is a partition of MouseEvent into the
;; following categories:
;; -- "button-down"   (interp: maybe select something)
;; -- "drag"          (interp: maybe drag something)
;; -- "button-up"     (interp: unselect something)
;; -- any other mouse event (interp: ignored)

;(define (mev-fn mev)
;  (cond
;    [(mouse=? mev "button-down") ...]
;    [(mouse=? mev "drag") ...]
;    [(mouse=? mev "button-up") ...]
;    [else ...]))

;; A MyKeyEvent is a partition of KeyEvent, which is one of
;; -- "t"  (interp: "tree" - add a tree to the world)
;; -- "n"  (interp: "node" - add a son for selected node)
;; -- "d"  (interp: "delete" - delete selected node and its subtree)
;; -- "u"  (interp: "upper" - delete all nodes and their subtree in upper half
;;                  of canvas)
;; -- any other KeyEvent (interp: ignore)

;; template:
;; my-kev-fn : MyKeyEvent -> ??
;(define (my-kev-fn kev)
;  (cond 
;    [(key=? kev "t") ...]
;    [(key=? kev "n") ...]
;    [(key=? kev "d") ...]
;    [(key=? kev "u") ...]
;    [else ...]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; mouse event handling - helpers and main functions

;; world-after-mouse-event : World Number Number MouseEvent -> World
;; GIVEN: A world, the current mouse coordinates and a mouse event
;; WHERE: mx and my must be within the boundaries of the canvas
;; RETURNS: the world that follows the given mouse event.
;; The mouse events that can affect the worlds are "button-up", "button-down"
;; and "drag".  All other mouse events are ignored.
;;
;; EXAMPLES: See tests
;;
;; STRATEGY:  structural decomposition on MyMouseEvent
(define (world-after-mouse-event w mx my mev)
  (cond    
    [(mouse=? mev "button-down") (world-after-button-down w mx my)]
    [(mouse=? mev "drag") (world-after-drag w mx my)]
    [(mouse=? mev "button-up")(world-after-button-up w mx my)]
    [else w]))

; helpers for mouse event handling

;; world-after-button-down : World Number Number -> World
;; GIVEN: A world and the current mouse coordinates
;; RETURNS: the world following a button-down at the given location.
;; If the button-down is inside a ball, return a world just like the
;; given one, except that that ball is selected.  If the button-down is outside
;; any balls, return a world just like the current one.
;;
;; EXAMPLES: see tests
;;
;; STRATEGY: structural decomposition on world
(define (world-after-button-down w mx my)
;  (if (in-any-node? mx my (world-roots w))
  (build-world-with-selected-subtree
    (world-mouse-x w)
    (world-mouse-y w)
    (world-roots w)))
;   w))


;; build-world-with-selected-subtree 
;;   Number Number ListOf<Ball> Boolean PosInt -> World
;; GIVEN: The current mouse coordinates, a list of balls, a paused state,
;; and a tick speed for the world
;; RETURNS: a world with any ball containing the coordinates now marked
;; as selected; other attributes of the world are set to the given values.
;; 
;; EXAMPLE:
;; 
;; STRATEGY: functional composition
(define (build-world-with-selected-subtree mx my roots)
  (make-world
   mx
   my
   (build-list-with-selected-nodes-at-position mx my roots)))


;; world-after-drag : World Number Number -> World
;; GIVEN: A world and the current mouse coordinates
;; RETURNS: the world following a drag at the given location.
;; if no balls in the world are selected, then return a world with the selection
;; mouse coordinates changed, but otherwise unchanged (balls, paused/unpaused,
;; and speed same as given world).  If any ball is selected, then return a world
;; with the new mouse selection position, and any selected balls moved by the
;; same delta as the mouse.
;;
;; EXAMPLES: See tests
;;
;; STRATEGY: structural decomposition on World
(define (world-after-drag w mx my)
 (build-world-with-moved-selected-subtree
   mx
   my
   (world-mouse-x w)
   (world-mouse-y w)
   (world-roots w)))

;; build-world-with-moved-selected-subtree : 
;;     Number Number Number Number ListOf<Node> -> World
;; GIVEN: x,y current mouse selection coordinates, previous x,y mouse 
;;  coordinates, and a list of nodes
;; RETURNS:  A world with the given mouse selection coordinates, a list 
;; of balls, a paused state and the given tick speed.  The returned world's list
;; of balls is the same as the given one, except any selected balls are moved by
;; the same delta as the mouse (ie: delta of current to previous coordinates).
;;   
;; EXAMPLE:
;;  (build-world-with-moved-selected-subtree
;;
;; STRATEGY: functional composition
(define (build-world-with-moved-selected-subtree 
         mx my prev-x prev-y roots)
  (make-world
   ;; Store the latest mouse selection position
   mx 
   my
   (build-list-with-selected-subtrees-moved 
    ; Pass delta for selected subtree position movement to balls
    (- mx prev-x)
    (- my prev-y)    
    roots)))

;; world-after-button-up : World Number Number -> World
;; GIVEN: A world and x,y coordinates for a mouse button-up click
;; RETURNS: the world following a mouse button up at the given location.
;; If no balls in the world are selected, then return a world just like the 
;; given one, other than the selected mouse coordinates are zero. If any ball
;; is selected, then return a world where those balls are unselected and the 
;; selection coordinates are zero.  The paused/unpaused state and tick speed
;; of the world is unchanged for button-up.
;;
;; EXAMPLES: See tests
;;
;; STRATEGY: structural decomposition on world
(define (world-after-button-up w mx my)
  (build-world-with-unselected-nodes 
   (world-mouse-x w)
   (world-mouse-y w)
   (world-roots w)))

;; build-world-with-unselected-nodes :
;;   Number Number ListOf<Node> -> World
;; GIVEN: The current mouse coordinates, a list of balls, a paused state, and
;; a world tick speed
;; RETURNS: A world with the given mouse coordinates, a list of balls, the
;; given paused state, and the given tick speed.  The world's list of balls
;; is the same as the given one, except any selected balls now unselected.
;;
;; EXAMPLE:
;;  (build-world-with-unselected-nodes 
;; 
;; STRATEGY: functional composition
(define (build-world-with-unselected-nodes mx my lst)
  (make-world
   mx
   my
   (build-list-with-unselected-node lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World MyKeyEvent -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow the given world
;; after the given key event.  
;; 
;; EXAMPLES: see tests below
;;
;; STRATEGY: structural decomposition on kev : MyKeyEvent
(define (world-after-key-event w kev)
  (cond
    [(key=? kev "t") (world-after-tree-key w)]
    [(key=? kev "n") (world-after-node-key w)]
    [(key=? kev "d") (world-after-delete-key w)]
    [(key=? kev "u") (world-after-upper-key w)]
    [else w]))

;; world-after-tree-key : World -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow the given world
;; after a "t" key event = a world with a new tree root
;; and other attributes unchanged.
;; 
;; EXAMPLES: see tests below
;;
;; STRATEGY: structural decomposition on World
(define (world-after-tree-key w)
  (build-world-with-new-tree
   (world-mouse-x w)
   (world-mouse-y w)
   (world-roots w)))

;; world-after-node-key : World -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow the given world
;; after a "n" key event.  If a node is currently selected,
;; and there's room for a new child node, then then a new
;; child node is added for the selected node.  If there is 
;; not room, or if no node is currently selected, the returned
;; world is the same as the given one.
;; 
;; EXAMPLES: see tests below
;;
;; STRATEGY: structural decomposition on World
(define (world-after-node-key w)
  (build-world-with-new-node
   (world-mouse-x w)
   (world-mouse-y w)
   (world-roots w)))

;; world-after-delete-key : World -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow the given world
;; after a "d" key event = a world the same as the given one,
;; except any selected nodes and their subtrees are removed.
;; 
;; EXAMPLES: see tests below
;;
;; STRATEGY: structural decomposition on World
(define (world-after-delete-key w)
  (build-world-with-deleted-node 
   (world-mouse-x w)
   (world-mouse-y w)
   (world-roots w)))

;; world-after-upper-key : World -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow the given world
;; after a "d" key event = a world the same as the given one,
;; except any nodes in the upper half of the canvas are deleted 
;; and their subtrees are removed.
;; 
;; EXAMPLES: see tests below
;;
;; STRATEGY: structural decomposition on World
(define (world-after-upper-key w)
  (build-world-with-deleted-upper-nodes
   (world-mouse-x w)
   (world-mouse-y w)
   (world-roots w)))