#lang racket/base
(require pict racket/class racket/draw racket/gui
         (only-in racket/math pi) racket/list/grouping)
(provide ring-sector standard-ring-sector π pie-chart-pict get-ranges)
(module+ test
  (require rackunit))
(define (π radians)  (* pi radians))

;; ring-sector: x y start end inner outer dc
;; draws a ring sector whose top-left corner is at x,y
;; start end (radians) inner outer (radius)
(define (ring-sector x y start end outer inner dc)
  (define diameter (* 2 outer))
  (define inner-diameter (* 2 inner))
  (define inset (- outer inner)) ;; needs to be inset by diff between inner and outer
  (define path (new dc-path%))
  (send path arc 0 0 diameter diameter start end)
  (send path arc inset inset inner-diameter inner-diameter end start #false) ;; counter-clockwise
  (send path close)
  (send dc draw-path path x y))

;; standard-ring-sector: x y start end inner outer dc
;; draws a ring sector whose top-left corner is at x,y
;; start end (radians) inner outer (radius)
(define (standard-ring-sector x y start end outer inner dc)
  (define (standard-rotation Θ) (+ (- Θ) (π 1/2)))
  (define standard-start (standard-rotation end))
  (define standard-end (standard-rotation start))
  (define diameter (* 2 outer))
  (define inner-diameter (* 2 inner))
  (define inset (- outer inner)) ;; needs to be inset by diff between inner and outer
  (define path (new dc-path%))
  (send path arc 0 0 diameter diameter standard-start standard-end)
  (send path arc inset inset inner-diameter inner-diameter standard-end standard-start #false) ;; counter-clockwise
  (send path close)
  (send dc draw-path path x y))

;; convert segment values into intervals
(define (values->cardinals ltb-values)
  (foldl (λ (i init-acc) (append init-acc (list (+ (last init-acc) i)))) '(0) ltb-values))

(define (cardinals->cardinalpairs ltb-values)
  (windows 2 1 ltb-values))

;; 
(define (get-ranges data)
  (define datavalues (map (λ (data-item-vector) (vector-ref data-item-vector 1)) data))
  (define sum (apply + datavalues))
  (define datavradians (map (λ (v) (* (π 2) (/ v sum))) datavalues))
  (cardinals->cardinalpairs (values->cardinals datavradians)))

;; pie-chart : (list-of vectors#(name value))  #:colors (listof colours)
;; sectors angles are proportional to value , there must be a color for each value
#; (pie-chart dc '(#(Eggs 1.5) #(Bacon 2.5) #(Pancakes 3.5)) #:colors '("red" "pink" "green" "orange" "blue"))
;; sectors start at π/2 and grow clockwise.
;; while pie-charts are traditionally ordered from largest to smallest that is left to the source data.
;; vs : values
(define (pie-chart dc diameter vs #:colours colours)
  ;(displayln (get-ranges vs))
  (for/list ([c colours]
             [sweep (get-ranges vs)])
    (send dc set-brush (new brush% [color c]))
    (standard-ring-sector 3 3 (first sweep) (second sweep) (/ diameter 2) 0 dc)))

;; pie-chart-pict
(define (pie-chart-pict diameter data #:colours colours)
  (dc (λ (dc dx dy)
        (let* ([old-brush (send dc get-brush)]
               [old-pen (send dc get-pen)])
          (send dc set-smoothing 'aligned)
          (send dc set-pen (new pen% [width 1] [color "slategray"]))
          (pie-chart dc diameter data #:colours colours)
          (send dc set-brush old-brush)
          (send dc set-pen old-pen))) diameter diameter))

(module+ test
  ;; Is run when using DrRacket or with `raco test`.
  ;; The code here does not run when this file is required by another module.
  (check-equal? (+ 2 2) 4))

(module+ main
  ;; Is run using DrRacket or the `racket` executable.
  (pie-chart-pict 110 '(#("Eggs" 1.5) #(Bacon 2.5) #(Pancakes 3.5)) #:colours '("orange" "pink" "green"))
  (pie-chart-pict 110 '(#(Eggs 7.5) #(Bacon 2.5) #(Pancakes 3.5) #("Beans" 2.5) #(Sausages 0.5)) #:colours '("red" "green" "blue" "pink" "orange"))
  
  
  ;; polar-area-diagram  TODO
  ;; sectors divide the disk/ring evenly 
  #; (polar-area-diagram '(#(Eggs 1.5) #(Bacon 2.5) #(Pancakes 3.5)) #:colors '("red" "pink" "green"))

  )
