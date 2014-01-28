;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |1.5-1 Traffic Light Template|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; A TLState is one of
;; -- "red"       interp: the traffic light is red
;; -- "yellow"    interp: the traffic light is yellow
;; -- "green"     interp: the traffic light is green

;; tls-fn : TLState -> ??
;(define (tls-fn tls)
;  (cond
;    [(string=? tls "red") ...]
;    [(string=? tls "yellow") ...]
;    [(string=? tls "green") ...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A TaxableIncome is a partition of NonNegInt into 
;; the following categories:
;; [0-10000)            interp: low income
;; [10000,20000)        interp: medium income
;; [20000,infinity)     interp: high income

;; taxable-income-fn : TaxableIncome -> ??
;; (define (taxable-income-fn income)
;;   (cond
;;     [(< income 10000) ...]
;;     [(and
;;        (<= 10000 income)
;;        (< income 20000)) ...]
;;     [(<= 20000 income) ...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An FallingCatKeyEvent is a partition of KeyEvent 
;; into the following categories:
;; -- " "                (interp: pause/unpause)
;; -- any other KeyEvent (interp: ignore)

;; fcke-fn : FallingCatKeyEvent -> ??
(define (fcke-fn kev)
  (cond
    [(string=? kev " ") ...]
    [else ...]))




