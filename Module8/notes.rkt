;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname notes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))
;^ this is recursive

;this is iterative

#;(define (fact2 n)
  (local ((define (loop-so-far left-to-go)
           (if (= left-to-go 0)
               so-far
               (loop (* so-far left-to-go)
                            (- left-to-go 1)))))
  (loop 1 n)))

(define (fib n)
  (if (< n 2)
      n 
      (+ (fib (- n 1))
         (fib (- n 2)))))

; Halting argument is mathmatical proof
; Halting measure is a shortcut - something that gets smaller 
; and the calculations stops when it gets to a certain number

; this is recursive 

; iterative version

(define (fib2 n)
  (local ((define (loop old older i)
           (if (= i n)
               (+ old older)
               (loop (+ old older) old (+ i 1)))))
    (loop 1 0 2)))

; halting measure:
; 1. show that n start at > 2 and is a positive integer
; 2. show that it gets smaller each time
; 3. stops at zero

; invariant - for this to work to build a fibonacci number
; n has to be greater than 2