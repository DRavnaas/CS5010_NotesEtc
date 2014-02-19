;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; CS5010
;; Problem Set 5
;; Exercise 1 (Trees)
;; Doyle Ravnaas (ravnaas.d@husky.neu.edu)
;; Stephen Court (court.s@husky.neu.edu)

;; PURPOSE:
;; The system allows the creation and manipulation of trees on a canvas.

;; HOW TO RUN?:
;; Run with (run 0)


(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require rackunit/text-ui)
;(require "extras.rkt")


;(provide run)
;(provide initial-world)
;(provide world-after-key-event)
;(provide world-after-mouse-event)
;(provide world-to-roots)
;(provide node-to-center)
;(provide node-to-sons)
;(provide node-to-selected?)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS
(define SQUARE-SIDE 20)

;; dimensions of the canvas
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 400)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

(define CANVAS-CENTER-X (/ CANVAS-WIDTH 2))
(define CANVAS-CENTER-Y (/ CANVAS-HEIGHT 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS



(define-struct world (mouse-x mouse-y roots))
;; A World is a (make-world Number Number ListOf<Node>)
;; Interpretation: 
;; mouse-x and mouse-y give the last known "selection" position of the mouse
;; in the world.  Both values are zero if nothing is selected in the world.
;; Roots is a list of trees in the world - it is empty if there are no trees.

;; template:
;; world-fn : World -> ??
;(define (world-fn w)
;  (... (world-mouse-x w)
;       (world-mouse-y w)
;       (world-roots w)
(define EMPTY-WORLD (make-world 0 0 empty))

;(define WORLD-WITH-ONE-UNSELECTED-NODE 
;  (make-world 0 0 (list (make-node ?????)))


;; A NodeMouseEvent is a partition of MouseEvent into the
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


;; A NodeKeyEvent is a partition of KeyEvent, which is one of
;; -- "t"  (interp: "tree" - add a new root node to the world)
;; -- "n"  (interp: "node" - add a son for selected node)
;; -- "d"  (interp: "delete" - delete selected node and its subtree)
;; -- "u"  (interp: "upper" - delete all nodes and their subtree in upper half
;;                  of canvas)
;; -- any other KeyEvent (interp: ignore)

;; examples for testing
(define NEW-ROOT-NODE-EVENT "t")
(define NEW-CHILD-EVENT "n")
(define DELETE-SELECTED-NODE-EVENT "d")
(define DELETE-UPPER-NODES-EVENT "u")
(define UNWANTED-KEY-EVENT "q")

;; template:
;; node-kev-fn : NodeKeyEvent -> ??
;(define (node-kev-fn kev)
;  (cond 
;    [(key=? kev "t") ...]
;    [(key=? kev "n") ...]
;    [(key=? kev "d") ...]
;    [(key=? kev "u") ...]
;    [else ...]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FUNCTIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Scene and image handling - functions that display the world and its elements

;; world-to-scene : World -> Scene
;; GIVEN: a world
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE: 
;;  (world-to-scene EMPTY-WORLD)
;;   = EMPTY-CANVAS
;; STRATEGY: structural decomposition on World
(define (world-to-scene w)
  (roots-to-image (world-roots w)))  


;; mouse event handling - helpers and main functions

;; world-after-mouse-event : World Number Number MouseEvent -> World
;; GIVEN: A world, the current mouse coordinates and a mouse event
;; WHERE: mx and my must be within the boundaries of the canvas
;; RETURNS: the world that follows the given mouse event.
;; The mouse events that can affect the worlds are "button-up", "button-down"
;; and "drag".  All other mouse events are ignored.
;;
;; EXAMPLES: See tests
;;
;; STRATEGY:  structural decomposition on NodeMouseEvent
(define (world-after-mouse-event w mx my mev)
  (cond    
    [(mouse=? mev "button-down") (world-after-button-down w mx my)]
    [(mouse=? mev "drag") (world-after-drag w mx my)]
    [(mouse=? mev "button-up")(world-after-button-up w mx my)]
    [else w]))

;; helpers for mouse event handling

;; world-after-button-down : World Number Number -> World
;; GIVEN: A world and the current mouse coordinates
;; RETURNS: the world following a button-down at the given location.
;; If the button-down is inside any node, return a world just like the
;; given one, except that those nodes are selected.  If the button-down 
;; is outside all nodes, return a world just like the current one.
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
;;   Number Number ListOf<Node> Boolean PosInt -> World
;; GIVEN: The current mouse coordinates, and a list of root nodes
;; RETURNS: a world with any node containing the coordinates now marked
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
;; If no nodes in the world are selected, then return a world with the selection
;; mouse coordinates changed, but otherwise unchanged.  If any node is selected, 
;; then return a world with the new mouse selection position, and any selected 
;; nodes moved by the same delta as the mouse (as determined by the previous
;; mouse coordinates stored in the world state).
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
;; RETURNS:  A world with the given mouse selection coordinates, and a list 
;; of root nodes.  The returned world's list of nodes is the same as the given 
;; one, except any selected nodes are moved by the same delta as the mouse 
;; (ie: delta of current to previous coordinates).
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
    ; Pass delta for selected subtree position movement to nodes
    (- mx prev-x)
    (- my prev-y)    
    roots)))

;; world-after-button-up : World Number Number -> World
;; GIVEN: A world and x,y coordinates for a mouse button-up click
;; RETURNS: the world following a mouse button up at the given location.
;; If no nodes in the world are selected, then return a world just like the 
;; given one, other than the selected mouse coordinates are zero. If any node
;; is selected, then return a world where the selected nodes are unselected and
;; the selection coordinates are zero.  
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
;; GIVEN: The current mouse coordinates, a list of nodes, a paused state, and
;; a world tick speed
;; RETURNS: A world with the given mouse coordinates, and a list of root nodes.
;; The world's list of nodes is the same as the given one, except any selected 
;; nodes now unselected.
;;
;; EXAMPLE:
;;  (build-world-with-unselected-nodes 
;;     100 100 two-roots-with-no-sons)
;;   = > A world with mouse coordinates set to 100 100, and no selected nodes
;; 
;; STRATEGY: functional composition
(define (build-world-with-unselected-nodes mx my roots)
  (make-world
   mx
   my
   (build-list-with-unselected-nodes roots)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; key event handling - helpers and main functions

;; world-after-key-event : World NodeKeyEvent -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow the given world
;; after the given key event.  
;; 
;; EXAMPLES: see tests below
;;
;; STRATEGY: structural decomposition on kev : NodeKeyEvent
(define (world-after-key-event w kev)
  (cond
    [(key=? kev NEW-ROOT-NODE-EVENT) (world-after-tree-key w)]
    [(key=? kev NEW-CHILD-EVENT) (world-after-node-key w)]
    [(key=? kev DELETE-SELECTED-NODE-EVENT) (world-after-delete-key w)]
    [(key=? kev DELETE-UPPER-NODES-EVENT) (world-after-upper-key w)]
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