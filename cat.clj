(ns catquine.core
(:refer-clojure :exclude [drop]))
 
 
(defn dup [stack queue]
[(cons (first stack) stack) queue])
 
(defn swap [stack queue]
[(cons (second stack) (cons (first stack) (nnext stack))) queue])
 
(defn call [stack queue]
[(next stack) (concat (first stack) queue)])
 
(defn drop [stack queue]
[(next stack) queue])
 
(defn dip [stack queue]
(let [quotation (first stack)
hide (second stack)]
[(nnext stack) (concat quotation [hide] queue)]))
 
 
 
 
(defn step [[stack queue]]
(let [word (first queue)
queue* (next queue)]
(if (fn? word)
(word stack queue*)
[(cons word stack) queue*])))
 
(defn run [& queue]
(loop [[stack queue :as state] [nil queue]]
(if (seq queue)
(recur (step state))
stack)))
 
 
(comment
 
;(run (list 5 10 dup))
 
(run 1 dup 2 swap)
(run 1 dup 2 swap drop)
(run 1 (list dup dup) call)
(run 1 2 (list dup) dip)
 
)