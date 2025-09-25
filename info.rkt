#lang info
(define collection "pie-chart")
(define deps '("base" "draw-lib" "gui-lib" "pict"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/pie-chart.scrbl" ())))
(define pkg-desc "Draw a pie chart as a pict")
(define version "0.0")
(define pkg-authors '(stephendegabrielle))
(define license '(Apache-2.0 OR MIT))
