;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname scene-and-nodes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Needs a review, more testing, more commenting
; but seems to draw nodes and lines at this point
; Haven't added "selected" or "no child room" behavior, but 
; hopefully that's two flags decomposed and passed to the place-node-on-image
; function.

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

; no branches on these
; roots are at 20 to make them easy to spot
(define world-with-one-root (make-world (list (make-node 20 20 empty))))
(define world-with-two-roots (make-world 
                             (list (make-node 20 20 empty)
                                   (make-node 100 20 empty))
                             ))


(define world-with-two-roots-one-branch
  (make-world 
   (list (make-node 20 20 
                    (list (make-node 20 50 empty)))
         (make-node 100 20 empty)))
                             )
(define world-with-two-roots-with-four-branches 
  (make-world 
      (list 
       ; first root - two children, two grandchildren
       (make-node 20 20 (list (make-node 20 50 empty)                               
                              (make-node 120 50 (list (make-node 120 100 empty)
                                                      (make-node 150 100 empty)))))
       ; second root - no grandchildren     
       (make-node 300 20 empty)))
      )

; functions

; STRATEGY: Structural decomposition on World
(define (world-to-scene w)
  (roots-to-scene 
   (world-roots w)
   EMPTY-CANVAS))

; STRATEGY: HOFC 
(define (roots-to-scene w-roots image)
  ; Fold each root (and its subtree) into an image
  (foldr subtree->image image w-roots))

       
; STRATEGY: HOFC (with a little decomposition on node)
(define (subtree->image a-node image)
  (place-node-on-image 
       ; x and y for the node
       (node-x-pos a-node)
       (node-y-pos a-node)
       ;(node-selected? a-node)
       ;(node-child-fit? a-node)
       
       ; build image for the rest of branch, including lines
       ; to this parent
       ; TODO this could be a foldr on node-branch?
       (foldr 
         ; Node Image -> Image
         (lambda (child image)
          (childnode->image
           ; pass a-node as the parent
            a-node
            ; current node in chidlen of a-node is the child
            child
            ; build an image of the child's branch
            (subtree->image child image)))
         ; go through this nodes chidren
         image (node-branch a-node))))
           
       ;     
       ;(branch->image 
       ; a-root
       ; (node-branch a-root) image)))


; STRATEGY:  Structural decomposition on Node
(define (childnode->image parent a-node image)
  ; draw the node, and a line back to its parent
  (place-node-on-image 
       (node-x-pos a-node)
       (node-y-pos a-node)
       ;(node-selected? a-node)
       ;(node-child-fit? a-node)
       (scene+line
        image
        (node-x-pos parent)
        (node-y-pos parent)
        (node-x-pos a-node)
        (node-y-pos a-node)
        "blue")))

; STRATEGY: Structural decomposition on Branch
; TODO - how to turn this into HOFC - likely a foldr into image
;(define (branch->image parent a-branch image)
;  (cond
;    [(empty? a-branch) image]
;    [else (childnode->image
;           parent
;           (first a-branch)
;           ; Image passed into node->image is the one 
;           ; built for all the branch nodes
;           (branch->image 
 ;           (first a-branch)
 ;           (rest a-branch) image))]))


; STRATEGY: Domain Knowledge
(define (place-node-on-image x y image)
  (place-image 
       UNSELECTED-NODE
       x y
        image))

;(world-to-scene (make-world empty))
;(world-to-scene world-with-one-node)

;(node-to-image (first (world-nodes world-with-one-node)) EMPTY-CANVAS)
(world-to-scene world-with-one-root)

(world-to-scene world-with-two-roots)

(world-to-scene world-with-two-roots-one-branch)

(world-to-scene world-with-two-roots-with-four-branches)