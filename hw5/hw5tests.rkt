#lang racket

(require "hw5.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.

; Note we have provided [only] 3 tests, but you can't run them until do some of the assignment.
; You will want more tests.

(require rackunit)

(define rlist (list (int 1) (int 2) (int 3)))
(define mlist (apair (int 1) (apair (int 2) (apair (int 3) (munit)))))
(define p (apair (add (int 10) (int 9)) (isgreater (int 10) (int 9))))

(define tests
  (test-suite
   "Homework 5 Tests"
   
   (check-equal? (racketlist->mupllist rlist) mlist)
   (check-equal? (mupllist->racketlist mlist) rlist)
   (check-equal? (eval-exp (add (int 2) (int 2))) (int 4) "add simple test")
   
   (check-equal? (eval-exp (isgreater (add (int 1) (int 1)) (int 1))) (int 1))
   
   (check-equal? (eval-exp (ifnz (isgreater (add (int 1) (int -1)) (int 1))
                                 (int 10) (int 9))) (int 9))
   
   (check-equal? (eval-exp (fun null "p" (int 1)))
                 (closure null (fun null "p" (int 1))))
   
   (check-equal? (eval-exp (mlet "test" (int 1) (add (var "test") (var "test"))))
                 (int 2))
   (check-equal? (eval-exp
                  (call (fun "bar" "p"(isgreater (first (var "p")) (second (var "p"))))
                                 (apair (int 7) (int 5)))) (int 1))

   (check-equal? (eval-exp p) (apair (int 19) (int 1)))
   (check-equal? (eval-exp (first p)) (int 19))
   (check-equal? (eval-exp (second p)) (int 1))
   (check-equal? (eval-exp (ismunit (munit))) (int 1))

   (check-exn (lambda (x) (string=? (exn-message x) "expression is not a MUPL pair"))
              (lambda () (eval-exp (first (int 1)))))
   
   (check-exn (lambda (x) (string=? (exn-message x) "bad MUPL expression: 12"))
              (lambda () (eval-exp 12)))
   
   (check-exn (lambda (x) (string=? (exn-message x) "MUPL addition applied to non-number"))
              (lambda () (eval-exp (add (int 2) (munit))))
              "add bad argument")
   
   (check-equal? (eval-exp (ifmunit (munit) (int 10) (int 9))) (int 10))
   (check-equal? (eval-exp (mlet* (list (cons "var1" (int 10)) (cons "var2" (int 9)))
                                  (add (var "var1") (var "var2")))) (int 19))
   (check-equal? (eval-exp (ifeq (int 1) (int 1) (int 10) (int 9))) (int 10))
   (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 10) (int 9))) (int 9))
   (check-equal? (eval-exp(call (call mupl-filter
                              (fun null "arg" (isgreater (var "arg") (int 0))))
                        (apair (int 1) (apair (int -1) (apair (int 3) (munit))))))
                 (apair (int 1) (apair (int 3) (munit))))
   
   (check-equal? (mupllist->racketlist
                  (eval-exp (call (call mupl-all-gt (int 9))
                                  (racketlist->mupllist 
                                   (list (int 10) (int 9) (int 15))))))
                 (list (int 10) (int 15))
                 "provided combined test using problems 1, 2, and 4")))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
