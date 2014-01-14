;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 01-1-bar-order) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; A Size is one of
;; -- "small"
;; -- "medium"
;; -- "large"

;; size-fn : Size -> ??
(define (size-fn s)
  (cond
    [(string=? s "small") ...]
    [(string=? s "medium") ...]
    [(string=? s "large") ...]))

;; A Year is a PosInt[1900,2020]
;; interp: the year.  Must be between 1900 and 2020.

;; Year is Scalar data, so we don't need a template.

;; A Vineyard is ... /to be filled in/

(define-struct coffee (size type milk?))
(define-struct wine (vineyard year))
(define-struct tea (size type))

;; A BarOrder is one of
;; -- (make-coffee Size String Boolean)
;;  interp: 
;;   size is the size of cup desired
;;   type is the origin of the coffee (as a string)
;;   milk? tells whether milk is desired.
;; -- (make-wine Vineyard Year)
;;  interp:
;;   vineyard is the origin of the grapes
;;   year is the year of harvest
;; -- (make-tea Size String)
;;  interp: 
;;   size is the size of cup desired
;;   type is the type of tea (as a string)

;; bo-fn : BarOrder -> ??
;(define (bo-fn order)
;  (cond
;    [(coffee? order) (...
;                      (coffee-size order)
;                      (coffee-type order)
;                      (coffee-milk? order))]
;    [(wine? order) (...
;                    (wine-vineyard order)
;                    (wine-year order))]
;    [(tea? order) (...
;                   (tea-size order)
;                   (tea-type order))]))

