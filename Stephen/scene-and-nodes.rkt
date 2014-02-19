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

; two rectangles with a line
;(scene+line scene-with-two-rectangles
;            50 50
;            100 100
;            "blue") 

;or we could use place-images for all the rectangles?

(define-struct node (x y branch))

(define-struct world (nodes))

(define empty-world (make-world empty))

(define world-with-one-node (make-world (list (make-node 20 20 empty))))

(define world-with-two-nodes (make-world 
                             (list (make-node 20 20 empty)
                                   (make-node 100 100 empty)
                             )))


(define world-with-three-nodes (make-world 
                             (list (make-node 20 20 
                                              (list (make-node 40 40 empty)))
                                   (make-node 100 100 empty)
                             )))


(define (world-to-scene w)
  (nodes-to-scene w EMPTY-CANVAS))

; not working with three node 
(define (nodes-to-scene w image)
  ; Fold each set of nodes into an image
  (foldr nodes-to-image image (world-nodes w)))

; This should use the template, not working on three node version
(define (nodes-to-image w-node image)
 (cond 
   ((empty? w-node) image)
   ((node? w-node) (node-to-image w-node image))
   (else (place-image 
          UNSELECTED-NODE
          (node-x w-node)
          (node-y w-node)
          (nodes-to-image (node-branch w-node) image)))))


(define (node-to-image a-node image)
  (place-image 
       UNSELECTED-NODE
       (node-x a-node)
       (node-y a-node)
        image))

;(world-to-scene (make-world empty))
;(world-to-scene world-with-one-node)

;(node-to-image (first (world-nodes world-with-one-node)) EMPTY-CANVAS)
(world-to-scene world-with-one-node)

(world-to-scene world-with-two-nodes)

; This one isn't right yet - not recursing subtree
(world-to-scene world-with-three-nodes)