;; making examples human-readable

;; a rocket simulation.  

;; Information Analysis:
;; We have a single rocket, which is at some altitude and is
;; travelling vertically at some velocity.
;; We should be able to pause the animation

;; a Rocket
(define-struct rocket (altitude velocity))

;; A Rocket is a (make-rocket Real Real)
;; INTERPRETATION:
;; altitude   is the rocket's height, in meters
;; velocity   is the rocket's velocity, in meters/sec upward

;; rocket-after-dt : Rocket Real -> Rocket
;; GIVEN: a rocket and and a time interval
;; RETURNS: the state of the rocket after the interval has passed

;; EXAMPLE:
;; (rocket-after-dt (make-rocket 100 30) 0) = (make-rocket 100 30)
;; (rocket-after-dt (make-rocket 100 30) 2) = (make-rocket 160 30)

;; BETTER
(define rocket-at-100 (make-rocket 100 30))
(define rocket-at-160 (make-rocket 160 30))

(rocket-after-dt rocket-at-100 0) = rocket-at-100
(rocket-after-dt rocket-at-100 2) = rocket-at-160


