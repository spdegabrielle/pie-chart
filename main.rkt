#lang racket/base
(require racket/class racket/draw racket/gui
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
(define (cardinals ltb-values)
  (foldl (λ (i init-acc) (append init-acc (list (+ (last init-acc) i)))) '(0) ltb-values))

(define (intervals ltb-values)
  (windows 2 1 ltb-values))
  


(define (pie-chart dc x y diameter list-of-colour-values)
  ;; list-of-colour-values --> list-of-colour-sectors

  ;; draw list of coloursectors 
  
  ;(for/list ([]))

  void)



(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (+ 2 2) 4))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (define data (list '("red" 13) '("pink" 23) '("yellow" 12) '("blue" 4)))
  (define datac (map (λ (d) (first d)) data)) ;; ordered list of colour strings
  (define datav (map (λ (d) (second d)) data)) ;; corresponding ordered list of values
  (define sum (apply + datav)) ;; sum of all values
  (define datavradians (map (λ (v) (* (π 2) (/ v sum))) datav)) ;; values as radians

  (define datavfract (map (λ (v)  (/ v sum)) datav)) ;; values as radians
  
  (printf "(1) is this 2π?  ~A~N" (apply + datavradians))
  (printf "(2) sum ~A~N" sum)
  (printf "(3) datav ~A~N" datav)
  (printf "(1) datavfract ~A~N" datavfract)
  (printf "(1) datavradians ~A~N" datavradians)
  
  (define ranges (intervals (cardinals datavradians))) ; ordered list of radian pairs



  
  (define target2 (make-bitmap 400 400  #:backing-scale 2))
  (define tdc2 (new bitmap-dc% [bitmap target2]))
  (send tdc2 set-smoothing 'aligned)
  (define old-brush (send tdc2 get-brush))
  (define old-pen (send tdc2 get-pen))
  (send tdc2 set-pen (new pen% [width 1] [color "slategray"]))

  (for/list ([c datac]
             [sweep ranges])
    (send tdc2 set-brush (new brush% [color c]))
    (ring-sector 3 3 (first sweep) (second sweep) 140 0 tdc2))

        
  ;(send tdc2 set-brush (new brush% [color "green"]))
  ;(ring-sector 3 3 (π 9/6) (π 12/6) 150 160 tdc2)
        
  (send tdc2 set-brush old-brush)
  (send tdc2 set-pen old-pen)

  (make-object image-snip% target2))
