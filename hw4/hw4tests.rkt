#lang racket

(require "hw4.rkt") 

;; A simple library for displaying a 2x3 grid of pictures: used
;; for fun in the tests below (look for "Tests Start Here").

(require (lib "graphics.rkt" "graphics"))

(open-graphics)

(define window-name "Programming Languages, Homework 4")
(define window-width 700)
(define window-height 500)
(define border-size 100)

(define approx-pic-width 200)
(define approx-pic-height 200)
(define pic-grid-width 3)
(define pic-grid-height 2)

(define (open-window)
  (open-viewport window-name window-width window-height))

(define (grid-posn-to-posn grid-posn)
  (when (>= grid-posn (* pic-grid-height pic-grid-width))
    (error "picture grid does not have that many positions"))
  (let ([row (quotient grid-posn pic-grid-width)]
        [col (remainder grid-posn pic-grid-width)])
    (make-posn (+ border-size (* approx-pic-width col))
               (+ border-size (* approx-pic-height row)))))

(define (place-picture window filename grid-posn)
  (let ([posn (grid-posn-to-posn grid-posn)])
    ((clear-solid-rectangle window) posn approx-pic-width approx-pic-height)
    ((draw-pixmap window) filename posn)))

(define (place-repeatedly window pause stream n)
  (when (> n 0)
    (let* ([next (stream)]
           [filename (cdar next)]
           [grid-posn (caar next)]
           [stream (cdr next)])
      (place-picture window filename grid-posn)
      (sleep pause)
      (place-repeatedly window pause stream (- n 1)))))

;; Tests Start Here
(define test1 (equal? (sequence 2 3 11) (list 3 5 7 9 11)))
(define xs (cons "Hello" (cons "Hi" null)))
(define test2 (equal? (string-append-map xs "suffix") (list "Hellosuffix" "Hisuffix")))
(define test3 (equal? "Hello" (list-nth-mod xs 2)))
(define test4and5 (equal? (list 1 2 3 4 5 -6 7) (stream-for-k-steps funny-number-stream 7)))
(define test6 (equal? (list "cats.jpg" "curry.jpg" "cats.jpg" "curry.jpg" "cats.jpg")(stream-for-k-steps cats-then-curry 5)))
(define test7 (equal? (list (cons 1 1) (cons 1 2) (cons 1 3) (cons 1 4)) (stream-for-k-steps (stream-add-one funny-number-stream) 4)))
(define test8 (equal? (list (cons 1 "a") (cons 2 "b") (cons 3 "a") (cons 1 "b") (cons 2 "a"))(stream-for-k-steps (cycle-lists (list 1 2 3) (list "a" "b")) 5)))
(define vec (vector (cons 1 2) (cons 2 3) (cons 7 8) 6))
(define test9 (equal? (cons 2 3) (vector-assoc 2 vec)))
(define ls (list (cons 1 2) (cons 2 3) (cons 6 0)))
(define cacher (caching-assoc ls 2))
(define a 7)
(while-greater 2 do (begin (set! a (- a 1)) (print "x") a))
(while-greater 2 do (begin (set! a (- a 1)) (print "x") a))

; These definitions will work only after you do some of the problems
; so you need to comment them out until you are ready.
; Add more tests as appropriate, of course.

(define nums (sequence 1 0 5))

(define files (string-append-map 
               (list "cats" "curry" "brett" "dan") 
               ".jpg"))

(define funny-test (stream-for-k-steps funny-number-stream 16))

; a zero-argument function: call (one-visual-test) to open the graphics window, etc.
(define (one-visual-test)
 (place-repeatedly (open-window) 0.5 (cycle-lists nums files) 27))

; similar to previous but uses only two files and one position on the grid
(define (visual-one-only)
  (place-repeatedly (open-window) 0.5 (stream-add-one cats-then-curry) 27))
