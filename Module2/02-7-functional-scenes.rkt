;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname images) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

(define scene0 (empty-scene 200 150))

(define scene1 
  (place-image 
   (circle 30 "outline" "red") 
   20 100 
   scene0)) 

(define scene2
  (place-image 
   (square 40 "solid" "green")
   40 120
   scene1))

(define scene3
  (place-image 
   (square 40 "solid" "green")
   40 120
   (place-image 
    (circle 30  "outline" "red") 
    20 100 
    (empty-scene 200 150))))

scene0
scene1
scene2
scene3


