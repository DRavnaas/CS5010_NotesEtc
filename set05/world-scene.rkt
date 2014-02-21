;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname world-scene) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; CS5010
;; Problem Set 5
;; Exercise 1 (Trees)
;; Stephen Court (court.s@husky.neu.edu)
;; Doyle Ravnaas (ravnaas.d@husky.neu.edu)

;; PURPOSE:
;; world-scene.rkt - Turns a world and its nodes into a scene

(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require rackunit/text-ui)
(require "extras.rkt")

(provide world->scene)

;; See Trees.rkt for data definitions

; PUT THESE IN TREES.RKT - start
(define-struct node (x-pos y-pos selected? child-fit? branch))
(define-struct world (mouse-x mouse-y roots))

(define EMPTY-CANVAS (empty-scene 400 400))
; end 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define UNSELECTED-NODE (rectangle 20 20 "outline" "green"))
(define SELECTED-NODE (rectangle 20 20 "solid" "green"))
(define NO-CHILD-FIT-NODE (rectangle 20 20 "solid" "red"))

;; WORLDS FOR TESTING
(define empty-world (make-world 0 0 empty))

(define world-with-one-root 
  (make-world 0 0 (list (make-node 20 20 false true empty))))
(define world-with-two-roots 
  (make-world 0 0 (list (make-node 20 20 false true empty)
                        (make-node 100 20 false true empty))))

(define world-with-two-roots-one-branch
  (make-world 0 0
              (list (make-node 20 20 false true 
                               (list (make-node 20 50 false true empty)))
                    (make-node 100 20 false true empty))))

(define world-with-two-roots-with-four-branches 
  (make-world 0 0
              (list 
               ; first root - two children, two grandchildren
               (make-node 20 20 
                          false true 
                          (list (make-node 20 50 false true empty)                               
                                (make-node 
                                 120 50 
                                 false true 
                                 (list 
                                  (make-node 120 100 false true empty)
                                  (make-node 150 100 false true empty)))))
               ; second root - no grandchildren     
               (make-node 300 20 false true empty))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FUNCTIONS

;; world->scene: World -> Image
;; GIVEN:
;; RETURNS:
;;
;; EXAMPLES:
;;
;; STRATEGY: Structural decomposition on World
(define (world->scene w)
  (roots->scene 
   (world-roots w)
   EMPTY-CANVAS))

;; roots->scene: ListOf<Node> -> Image
;; GIVEN:
;; RETURNS:
;;
;; EXAMPLES:
;;
;; STRATEGY: HOFC 
(define (roots->scene w-roots image)
  ; Each root is a branch - fold together those images into the given image
  (foldr branch->image image w-roots))


;; branch->image : Branch -> Image
;; GIVEN:
;; RETURNS:
;;
;; EXAMPLES:
;;
;; STRATEGY: HOFC and Structural Decomposition on Node
(define (branch->image a-node image)
  (place-node-on-image 
   (node-x-pos a-node)
   (node-y-pos a-node)
   (node-selected? a-node)
   (node-child-fit? a-node)
   ; Build an image from the children of this node (with branch lines)
   (foldr 
    ; Node Image -> Image
    ; GIVEN: A node and an image
    ; RETURNS: An image that represent the given image with the
    ;          image representation of the chile node on it - ie:
    ;          a square with a line back to the parent.
    ; STRATEGY: functional composition
    (lambda (child image)
      (childnode->image
       ; a-node is the current node = parent of each of its child nodes
       a-node
       ; current node in chidlen of a-node is the child
       child
       ; build an image of the child's branch
       (branch->image child image)))
    
    ; base item is the given image, iterate through a-node's children
    image (node-branch a-node))))

;; childnode->image : Node Node Image -> Image
;; GIVEN:
;; RETURNS:
;;
;; EXAMPLES:
;;
;; STRATEGY:  Structural decomposition on Node
(define (childnode->image parent child image)
  ; draw the node, and a line back to its parent
  (place-node-on-image
   ; Pass in various attributes of the child
   (node-x-pos child)
   (node-y-pos child)
   (node-selected? child)
   (node-child-fit? child)
   ; Draw a blue line on the given image from the parent to th child
   (scene+line
    image
    (node-x-pos parent)
    (node-y-pos parent)
    (node-x-pos child)
    (node-y-pos child)
    "blue")))


;; place-node-on-image: Number Number Boolean Boolean Image -> Image
;; GIVEN:
;; RETURNS:
;;
;; EXAMPLES:
;; (place-node-on-image 50 50 false true EMPTY-CANVAS)
;;   -> A green rectangle outline, centered at 50, 50 on a 400 x 400 canvas
;; (place-node-on-image 50 50 true false EMPTY-CANVAS)
;;   -> A green solid rectangle, centered at 50, 50 on a 400 x 400 canvas
;; (place-node-on-image 50 50 false true EMPTY-CANVAS)
;;   -> A red solid rectangle, centered at 50, 50 on a 400 x 400 canvas
;; 
;; STRATEGY: Domain Knowledge
(define (place-node-on-image x y selected? child-fit? image)
  (place-image 
   ; Determine which node image to use
   (if selected? 
       (if child-fit? NO-CHILD-FIT-NODE SELECTED-NODE)
       UNSELECTED-NODE)
   x y
   image))

; tests

;; IMAGES FOR TESTING

; one rectangle on a canvas
(define scene-with-one-rectangle-outline
  (place-image (rectangle 20 20 "outline" "green")
               20 20 
               EMPTY-CANVAS))

; two rectangles on a canvas
(define scene-with-two-rectangles-outline
  (place-image (rectangle 20 20 "outline" "green")
               100 20 
               scene-with-one-rectangle-outline))

(check-equal?
 (world->scene empty-world)
 EMPTY-CANVAS
 "An empty world is an empty canvas")

(check-equal?
  (world->scene world-with-one-root)
  scene-with-one-rectangle-outline
  "One unselected node at 20,20 didn't look like the expected image")

(check-equal?
 (world->scene world-with-two-roots)
 scene-with-two-rectangles-outline
 "Two unselected roots at 20,20 and 50/20 didn't look like expected")

;(world->scene world-with-two-roots-one-branch)
 

;(world->scene world-with-two-roots-with-four-branches)

; various for debugging
;(world->scene (make-world empty))
;(world->scene world-with-one-node)

;(node-to-image (first (world-nodes world-with-one-node)) EMPTY-CANVAS)

