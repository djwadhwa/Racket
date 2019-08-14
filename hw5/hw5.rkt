;; CSE341, Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct isgreater (e1 e2)    #:transparent) ;; if e1 > e2 then 1 else 0
(struct ifnz (e1 e2 e3) #:transparent) ;; if not zero e1 then e2 else e3
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
(struct apair   (e1 e2) #:transparent) ;; make a new pair
(struct first   (e)     #:transparent) ;; get first part of a pair
(struct second  (e)     #:transparent) ;; get second part of a pair
(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then 1 else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;; Problem 1
(define (racketlist->mupllist rlist)
  (cond [(null? rlist) (munit)]
         [#t (apair (car rlist) (racketlist->mupllist (cdr rlist)))]))

;; CHANGE (put your solutions here)
(define (mupllist->racketlist mlist)
  (cond [(munit? mlist) null]
         [#t (cons (apair-e1 mlist) (mupllist->racketlist (apair-e2 mlist)))]))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]
        [(munit? e) e]
        [(closure? e) e]
        [(isgreater? e)
         (let ([v1 (eval-under-env (isgreater-e1 e) env)]
               [v2 (eval-under-env (isgreater-e2 e) env)])
           (if (int? v1)
               (if (int? v2)
                   (if (> (int-num v1) (int-num v2))
                       (int 1)
                       (int 0))
                   (error "second expression is not a MUPL integer"))
               (error "first expression is not a MUPL integer")))]
        [(ifnz? e)
         (let ([v1 (eval-under-env (ifnz-e1 e) env)])
           (if (int? v1)
               (if (not (= (int-num v1) 0))
                   (eval-under-env (ifnz-e2 e) env)
                   (eval-under-env (ifnz-e3 e) env))
               (error "first expression is not a MUPL integer")))]
        [(fun? e) (closure env e)]
        [(mlet? e) (let* ([v (eval-under-env (mlet-e e) env)]
                          [new-env (cons (cons (mlet-var e) v) env)])
                     (eval-under-env (mlet-body e) new-env))]
        [(call? e) (let ([v1 (eval-under-env (call-funexp e) env)]
                         [v2 (eval-under-env (call-actual e) env)])
                     (if (closure? v1)
                         (let ([function (closure-fun v1)]
                               [environment (closure-env v1)])
                           (if (null? (fun-nameopt function))
                               (eval-under-env (fun-body function) (cons (cons (fun-formal function) v2) environment))
                               (eval-under-env (fun-body function) (cons (cons (fun-nameopt function) v1) (cons (cons (fun-formal function) v2) environment)))))
                         (error "first expression not a MUPL closure")))]
        [(apair? e) (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(first? e) (let ([v (eval-under-env (first-e e) env)])
                      (if (apair? v)
                          (apair-e1 v)
                          (error "expression is not a MUPL pair")))]
        [(second? e) (let ([v (eval-under-env (second-e e) env)])
                       (if (apair? v)
                           (apair-e2 v)
                           (error "expression is not a MUPL pair")))]
        [(ismunit? e) (if (munit? (eval-under-env (ismunit-e e) env))
                          (int 1)
                          (int 0))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3
(define (ifmunit e1 e2 e3)
  (ifnz (ismunit e1) e2 e3))

(define (mlet* bs e2)
  (if (null? bs)
      e2
      (mlet (car (car bs)) (cdr (car bs)) (mlet* (cdr bs) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2)) 
         (ifnz (isgreater (var "_x") (var "_y"))
               e4
               (ifnz (isgreater (var "_y") (var "_x"))
                     e4
                     e3))))
          
;; Problem 4
;fix mupl-filter
(define mupl-filter
  (fun null "func"
       (fun "recursive" "ls"
            (ifnz (ismunit (var "ls" ))
                  (munit)
                  (ifnz (call (var "func") (first (var "ls")))
                        (apair (first (var "ls")) (call (var "recursive") (second (var "ls"))))
                        (call (var "recursive") (second (var "ls"))))))))

(define mupl-all-gt
  (mlet "filter" mupl-filter (fun null "num" (call (var "filter") (fun null "val" (isgreater (var "val") (var "num")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
