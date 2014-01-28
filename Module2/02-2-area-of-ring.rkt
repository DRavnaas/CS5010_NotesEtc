;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname area-of-ring1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; area-of-ring1.rkt

(require rackunit)
(require rackunit/text-ui)

;; area-of-ring : Number Number -> Number
;; GIVEN: the inner and outer radii of a ring
;; RETURNS: the area of the ring
;; EXAMPLE:
;; (area-of-ring 2 3) = 9*pi - 4*pi = 5*pi    

;; STRATEGY: function composition
(define (area-of-ring inner outer)
  (- (area-of-circle outer)
     (area-of-circle inner)))

(define-test-suite area-of-ring-tests
  (check-=
    (area-of-ring 3 3)
    0
    0.01
    "(area-of-ring 3 3) should be 0")
  (check-=
    (area-of-ring 2 3)
    (* 5 pi)
    0.01
    "(area-of-ring 2 3) should be 5*pi"))

;; can't run these tests until after area-of-circle is defined
; (run-tests area-of-ring-tests)


;; area-of-circle : Number -> Number
;; GIVEN: the radius of a circle
;; RETURNS: its area
;; EXAMPLE:
;; (area-of-circle 2) = 4*pi
;; (area-of-circle 3) = 9*pi
;; (area-of-circle 4) = 16*pi
;; STRATEGY: domain knowledge
(define (area-of-circle radius)
  (* pi radius radius))

;;; tests for area-of-circle
;; pi happens to be predefined in BSL (you could look it up!)

(define-test-suite area-of-circle-tests

  (check-= (area-of-circle 2) (* 4 pi) 0.01
    "radius = 2 should give area = 4*pi")

  (check-= (area-of-circle 3) (* 9 pi) 0.01
    "radius = 3 should give area = 9*pi")

  (check-= (area-of-circle 4) (* 16 pi) 0.01
    "radius = 4 should give area = 16*pi"))

(run-tests area-of-circle-tests)
(run-tests area-of-ring-tests)




