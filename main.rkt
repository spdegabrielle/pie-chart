#lang racket/base
(require pict racket/class racket/draw racket/gui
         (only-in racket/math pi) racket/list/grouping)
(provide ring-sector π pie-chart)


(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

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
(define (pie-chart dc vs #:colours colours)
  (displayln (get-ranges vs))
  (for/list ([c colours]
             [sweep (get-ranges vs)])
    (send dc set-brush (new brush% [color c]))
    (ring-sector 3 3 (first sweep) (second sweep) 140 0 dc)))

;; pie-chart-pict

(define (pie-chart-pict diameter data #:colours colours)
  (dc (λ (dc dx dy)
        (let* ([old-brush (send dc get-brush)]
               [old-pen (send dc get-pen)])
          (send dc set-smoothing 'aligned)
          (send dc set-pen (new pen% [width 1] [color "slategray"]))
          ;; draw the owl
          (pie-chart dc data #:colours colours)
          (displayln "draw the owl2")
          (send dc set-brush old-brush)
          (send dc set-pen old-pen))

        ) diameter diameter))


(module+ test
  ;; Is run when using DrRacket or with `raco test`.
  ;; The code here does not run when this file is required by another module.
  (check-equal? (+ 2 2) 4))

(module+ main
  ;; Is run using DrRacket or the `racket` executable.

  ;; drawing
  (let* ([target3 (make-bitmap 400 400  #:backing-scale 2)]
         [tdc3 (new bitmap-dc% [bitmap target3])]
         [old-brush (send tdc3 get-brush)]
         [old-pen (send tdc3 get-pen)])
    (send tdc3 set-smoothing 'aligned)
    (send tdc3 set-pen (new pen% [width 1] [color "slategray"]))
    ;; draw the owl
    (pie-chart tdc3 '(#(Eggs 1.5) #(Bacon 2.5) #(Pancakes 3.5)) #:colours '("red" "pink" "green"))
    
    (send tdc3 set-brush old-brush)
    (send tdc3 set-pen old-pen)
    (make-object image-snip% target3))



  (pie-chart-pict 410 '(#(Eggs 1.5) #(Bacon 2.5) #(Pancakes 3.5)) #:colours '("orange" "pink" "green"))
        

  ;; (ring-sector x y start end outer inner dc)
  (define target2 (make-bitmap 400 400  #:backing-scale 2))
  (define tdc2 (new bitmap-dc% [bitmap target2]))
  (send tdc2 set-smoothing 'aligned)
  (define old-brush (send tdc2 get-brush))
  (define old-pen (send tdc2 get-pen))
  
  (send tdc2 set-pen (new pen% [width 1] [color "slategray"]))
  (send tdc2 set-brush (new brush% [color "red"]))
  (ring-sector 3 3 (π 0/6) (π 7/6) 150 100 tdc2)
        
  (send tdc2 set-brush (new brush% [color "blue"]))
  (ring-sector 3 3 (π 7/6) (π 9/6) 140 100 tdc2)

          
  (send tdc2 set-brush (new brush% [color "yellow"]))
  (ring-sector 3 3 (π 11/6) (π 1/2) 140 10 tdc2)
        
  (send tdc2 set-brush (new brush% [color "green"]))
  (ring-sector 3 3 (π 3/6) (π 12/6) 150 160 tdc2)
        
  (send tdc2 set-brush old-brush)
  (send tdc2 set-pen old-pen)

  (make-object image-snip% target2)
  
  ;; polar-area-diagram
  ;; sectors divide the disk/ring evenly 
  #; (polar-area-diagram '(#(Eggs 1.5) #(Bacon 2.5) #(Pancakes 3.5)) #:colors '("red" "pink" "green"))

  )
