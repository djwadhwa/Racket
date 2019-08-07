#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below


(define (sequence spacing low high)
  (cond [(> low high) null]
        [#t (cons low (sequence spacing (+ low spacing) high))]))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (letrec ([treverse (lambda (xs i) (if (= i 0) (car xs) (treverse (cdr xs) (- i 1))))])
  (cond [(> 0 n) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (treverse xs (remainder n (length xs)))])))

(define (stream-for-k-steps s k)
  (let* ([pr (s)])
    (if (= k 0)
      null
      (cons (car pr) (stream-for-k-steps (cdr pr) (- k 1))))))

(define funny-number-stream
  (letrec ([f (lambda (x) (if (= (remainder x 6) 0)
                              (cons (- 0 x) (lambda() (f(+ x 1))))
                              (cons x (lambda() (f(+ x 1))))))])
    (lambda() (f 1))))

(define cats-then-curry
  (letrec ([f (lambda (x) (if (= (remainder x 2) 0)
                              (cons "cats.jpg" (lambda() (f(+ x 1))))
                              (cons "curry.jpg" (lambda() (f(+ x 1))))))])
    (lambda () (f 0))))

(define (stream-add-one s)
  (let* ([pr (s)])
    (lambda () (cons (cons 1 (car pr)) (stream-add-one (cdr pr))))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (x)
              (cons (cons (list-nth-mod xs x) (list-nth-mod ys x))
                          (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  ())