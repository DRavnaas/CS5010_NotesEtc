;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname scene-and-nodes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require rackunit/text-ui)

(define EMPTY-CANVAS (empty-scene 400 400))

(define UNSELECTED-NODE (rectangle 20 20 "outline" "green"))
(define SELECTED-NODE (rectangle 20 20 "solid" "green"))
(define NO-CHILD-FIT-NODE (rectangle 20 20 "solid" "red"))

; one rectangle on a canvas
(define scene-with-one-rectangle
  (place-image (rectangle 20 20 "outline" "green")
               100 100 
               EMPTY-CANVAS))

; two rectangles on a canvas
(define scene-with-two-rectangles
  (place-image (rectangle 20 20 "outline" "green")
  50 50 
  scene-with-one-rectangle))

; An example of drawing a line between two rectangles
;(scene+line scene-with-two-rectangles
;            50 50
;            100 100
;            "blue") 

; Note to Steve - 
; There's an alternative of using place-images, rather than place-image;
; it takes a list of images and a list of positions.  It means we'd
; have to write a function to gather up a list of rectangles and positions
; from our roots and then pass that in "at the top".  
; Somehow that seems like it doesn't follow the recursive
; pattern quite as well, so I'm instead building a composite image with a
; place-image call for each node.

; Simplified data definitions for just working on scene
; TODO - add selected? and node-child-fit? handling
(define-struct node (x-pos y-pos branch))
(define-struct world (roots))
; roots is a ListOf<Node>

; examples for manual testing
(define empty-world (make-world empty))
(define world-with-one-node (make-world (list (make-node 20 20 empty))))
(define world-with-two-nodes (make-world 
                             (list (make-node 20 20 empty)
                                   (make-node 100 100 empty))
                             ))

(define world-with-three-nodes (make-world 
                                (list (make-node 20 20 
                                        (list (make-node 40 40 empty)))
                                      (make-node 100 100 empty)))
                             )
(define world-with-two-roots-with-branches 
  (make-world 
      (list 
       ; first root - two children, two grandchildren
       (make-node 20 20 (list (make-node 40 40 empty)                               
                              (make-node 200 200 (list (make-node 250 250 empty)
                                                       (make-node 100 100 empty)))))
       ; second root - no grandchildren     
       (make-node 300 50 empty)))
      )

; functions

; STRATEGY: Structural decomposition on World
(define (world-to-scene w)
  (nodes-to-scene 
   (world-roots w)
   EMPTY-CANVAS))

; STRATEGY: HOFC 
(define (nodes-to-scene w-nodes image)
  ; Fold each root (and its subtree) into an image
  (foldr node->image image w-nodes))


; STRATEGY: Structural decomposition on Node
(define (node->image a-node image)
  (place-node-on-image 
        (node-x-pos a-node)
       (node-y-pos a-node)
       ;(node-selected? a-node)
       ;(node-child-fit? a-node)
       
       ; build image from rest of branch
       (branch->image (node-branch a-node) image)))

; STRATEGY: Structural decomposition on Branch
(define (branch->image a-branch image)
  (cond
    [(empty? a-branch) image]
    [else (node->image (first a-branch)
                       ; Image passed into node->image is the one 
                       ; built for all the branch nodes
                       (branch->image (rest a-branch) image))]))

; STRATEGY: Domain Knowledge
(define (place-node-on-image x y image)
  (place-image 
       UNSELECTED-NODE
       x y
        image))

;(world-to-scene (make-world empty))
;(world-to-scene world-with-one-node)

;(node-to-image (first (world-nodes world-with-one-node)) EMPTY-CANVAS)
(world-to-scene world-with-one-node)

(world-to-scene world-with-two-nodes)

(world-to-scene world-with-three-nodes)

(world-to-scene world-with-two-roots-with-branches)