;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname build-world-after-key-helpers) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(require rackunit)
(require rackunit/text-ui)
(require "extras.rkt")

(provide build-world-with-new-node)
(provide build-world-with-new-tree)
(provide build-world-delete-node)
(provide build-world-with-deleted-upper-nodes)

; PUT THESE IN TREES.RKT - start
(define-struct node (x-pos y-pos selected? child-fit? branch))
(define-struct world (mouse-x mouse-y roots))
(define NEW-NODE-X 200)
(define NEW-NODE-Y 10)

; A new tree root
(define NEW-TREE-ROOT-AT-TOP-CENTER
 (make-node NEW-NODE-X NEW-NODE-Y false true empty))

(define SELECTED-TREE-ROOT-AT-TOP-CENTER
 (make-node NEW-NODE-X NEW-NODE-Y true true empty))

(define SELECTED-TREE-ROOT-AT-TOP-LEFT
 (make-node 20 20 true false empty))

;; build-world-with-new-tree : Number Number ListOf<Node>
;; GIVEN: An x and y coordinates representing the current mouse position
;;        in a world and a list of nodes representing a set of trees
;; RETURNS: A world built with the given attributes, with a new tree root
;; added to the input set of trees.
;;
;; EXAMPLES:
;;  (build-world-with-new-tree 10 10 empty)
;;   = (make-world 10 10 (list NEW-TREE-ROOT-AT-TOP-CENTER))
;;
;; STRATEGY: functional composition
(define (build-world-with-new-tree x y roots)
  (make-world
   x y
   (build-roots-with-new-tree roots)))

;; build-world-with-new-node Number Number ListOf<Node>
;; GIVEN: An x and y coordinates representing the current mouse position
;;        in a world and a list of nodes representing a set of trees
;; RETURNS: Either a world with the given input attributes (= node could not be 
;; added), or a world with the given attributes and one new node.  A node is 
;; added to a selected node in the input list if there is "room" to add a child 
;; node.
;;
;; EXAMPLES:
;;  (build-world-with-new-node 50 50 empty)
;;   => (make-world 50 50 empty) 
;;  (build-world-with-new-tree 50 50 (list NEW-TREE-ROOT-AT-TOP-CENTER))
;;   => (make-world 50 50 (list NEW-TREE-ROOT-AT-TOP-CENTER))
;;  (build-world-with-new-tree 
;;     NEW-NODE-X NEW-NODE-Y (list SELECTED-TREE-ROOT-AT-TOP-LEFT))
;;  (build-world-with-new-tree
;;     NEW-NODE-X NEW-NODE-Y (list SELECTED-TREE-ROOT-AT-TOP-LEFT))
;;>
;;
;; STRATEGY: functional composition
(define (build-world-with-new-node x y roots)
  (make-world
   x y
   (build-roots-with-new-node roots)))
  
;; build-world-with-new-node Number Number ListOf<Node>
;; GIVEN: An x and y coordinates representing the current mouse position
;;        in a world and a list of nodes representing a set of trees
;; RETURNS:
;;
;; EXAMPLES:
;;
;; STRATEGY: functional composition
(define (build-world-delete-node x y roots)
  (make-world
   x y
   (build-roots-with-delete-node roots)))
  
;; build-world-with-new-node Number Number ListOf<Node>
;; GIVEN: An x and y coordinates representing the current mouse position
;;        in a world and a list of nodes representing a set of trees
;; RETURNS:
;;
;; EXAMPLES:
;;
;; STRATEGY: functional composition
(define (build-world-with-deleted-upper-nodes x y roots)
  (make-world
   x y
   (build-roots-with-deleted-upper-nodes roots)))

;; build-roots-with-new-tree: ListOf<Node> -> ListOf<Node>
;; GIVEN: a list of nodes, representing root nodes
;; RETURNS: a list consisting of one newly added root and the 
;; input list of nodes
;;
;; EXAMPLES:
;;  (build-roots-with-new-tree empty)
;;  => (list NEW-TREE-ROOT-AT-TOP-CENTER))
;;
;; STRATEGY: functional composition
(define (build-roots-with-new-tree roots)
  ; Build a new list of roots with one new root added
  (cons NEW-TREE-ROOT-AT-TOP-CENTER roots))


;; build-roots-with-new-node : ListOf<Node> -> ListOf<Node>
;; GIVEN:
;; RETURNS:
;;
;; EXAMPLES:
;;
;; STRATEGY: functional composition
(define (build-roots-with-new-node roots)
  ; For each branch, see if a node should be added
  (map maybe-add-new-child-to-branch (roots)))

;; build-roots-with-delete-node : ListOf<Node> -> ListOf<Node>
;; GIVEN:
;; RETURNS:
;;
;; EXAMPLES:
;;
;; STRATEGY: functional composition
(define (build-roots-with-delete-node roots)
  (roots))

;; build-roots-with-deleted-upper-nodes : ListOf<Node> -> ListOf<Node>
;; GIVEN:
;; RETURNS:
;;
;; EXAMPLES:
;;
;; STRATEGY: functional composition
(define (build-roots-with-deleted-upper-nodes roots)
  (roots))

;; maybe-add-new-child-to-branch : ListOf<Node> -> ListOf<Node>
;; GIVEN:
;; RETURNS:
;;
;; EXAMPLES:
;;
;; STRATEGY: Structural decomposition on Branch 
(define (maybe-add-new-child-to-branch a-branch)  
  ; for each node in the branch, see if we should add a child
  (map maybe-add-new-child-to-node a-branch))

;; maybe-add-new-child-to-node : Node -> Node
;; GIVEN: 
;; RETURNS:
;;
;; EXAMPLES:
;;
;; STRATEGY: Structural decomposition on Node
(define (maybe-add-new-child-to-node a-node)
  ; Evaluate adding a child to this node, and then evaluate its children.
  ; To add a child, node must be selected and have room
  (if (and (node-selected? a-node) (node-child-fit? a-node))
      ; Selected and has room - Add a child
      (node-with-new-child a-node)
      ; Not this node, build a new node processing this node's children
      (make-node 
       (node-x-pos a-node)
       (node-y-pos a-node)
       (maybe-add-new-child-to-branch (node-branch a-node)))))

;; node-with-new-child : Node -> Node
;; GIVEN: 
;; RETURNS:
;;
;; EXAMPLES:
;;
;; STRATEGY: functional composition
; Add a child to the node - we already  it's selected and will fit
(define (node-with-new-child a-node)
  (make-node 
    (node-x-pos a-node)
    (node-y-pos a-node)
    (node-selected? a-node)
    (node-child-fit? a-node)
    (add-child-node-to-list 
     (node-x-pos a-node)
     (node-y-pos a-node)
     (node-branch a-node))))

;; add-child-node-to-list : Number Number ListOf<Node> -> ListOf<Node>
;; GIVEN: 
;; RETURNS:
;;
;; EXAMPLES:
;;
;; STRATEGY: functional composition
;;>  This needs to figure out what 
(define (add-child-node-to-list parent-x parent-y a-branch)
  ; Add a child directly under the parent, or left of the leftmost child
  (cons (build-child-node 
                 (- (find-leftmost-child-x parent-x a-branch) 20)
                 (+ parent-y 60)
                 a-branch)))
             
;; find-leftmost-child-x : Number ListOf<Node> -> Number
;; GIVEN: 
;; RETURNS:
;;
;; EXAMPLES:
;;
;; STRATEGY: HOFC
(define (find-leftmost-child-x parent-x a-branch)
  (cond
    ; If this is the first child - add it right below the parent
    [(empty? a-branch) parent-x]
    ; Otherwise, check all siblings to find the lowest x position
    ;> change to constants
    [ else (foldr 
            (lambda (a-node low-so-far) 
              (min (node-x-pos a-node) low-so-far))
            400 a-branch)]))
    
;; build-child-node : Number Number -> Node
;; GIVEN: 
;; RETURNS:
;;
;; EXAMPLES:
;;
;; STRATEGY: functional composition
(define (build-child-node x y)
  (make-node   
   x
   y 
   false 
   ; Set child-fit? based on x and y
   ;> Change this to constants.
   (and (> 10 x) (< 390 x) (> 10 y) (< 390 y))))
