(ns lisp.interpreter
  (:require [clojure.core.match :refer (match)]))


(defn eval-expr [expr env]
  (match [expr]
    [n :guard number?] n
    [b :guard boolean?] b
    [(['zed? e] :seq)] (zero? (eval-expr e env))
    [(['add1 e] :seq)] (inc (eval-expr e env))
    [(['sub1 e] :seq)] (dec (eval-expr e env))
    [(['times e1 e2] :seq)] (* (eval-expr e1 env)
                               (eval-expr e2 env))
    [(['decide t c a] :seq)] (if (eval-expr t env)
                               (eval-expr c env)
                               (eval-expr a env))
    [x :guard symbol?] (env x)
    [(['lambda ([x] :seq) body] :seq)]
    (fn [arg] (eval-expr body
                         (fn [y] (if (= y x)
                                   arg
                                   (env y)))))
    [([rator rand] :seq)] ((eval-expr rator env)
                           (eval-expr rand env))))

;; Just number
(eval-expr
 '5 (fn [_] "Error!")) ;; => 5

;; add1
(eval-expr
 '(add1 (add1 5)) (fn [_] "Error!")) ;; => 7

;; sub1
(eval-expr
 '(sub1 5) (fn [_] "Error!")) ;; => 4

;; add1 and sub1
(eval-expr
 '(add1 (sub1 5)) (fn [_] "Error!"))

;; times
(eval-expr
 '(times 5 10) (fn [_] "Error!")) ;; => 50

;; if
(eval-expr
 '(decide false 1 2) (fn [_] "Error!")) ;; => 2

;; Symbol alone gets an Error
(eval-expr
 'x (fn [_] "Error!")) ;; => "Error!"

;; Lambda with one argument, returns the function
(eval-expr
 '(lambda [x] x) (fn [_] "Error!")) ;; => #object[sci.impl.fns$fun$arity_1__7325 0x47a420b2 "sci.impl.fns$fun$arity_1__7325@47a420b2"]

;; Construct proc and then apply it
(eval-expr
 '((lambda [x] (add1 (add1 x))) 5) (fn [_] "Error!")) ;; => 7

;; Testing for zero
(eval-expr
 '(zed? 0) (fn [_] "Error!")) ;; => true




