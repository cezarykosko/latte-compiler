(ns latte-compiler.util
  (:require [clojure.algo.monads :as m]
            [clojure.core.match :refer [match]]))

; coll

(defn third
  [coll]
  (fnext (next coll)))

(defn fourth
  [coll]
  (fnext (nnext coll)))

; IO

(defn println-err
  [msg]
  (binding [*out* *err*]
    (println msg)))

(defn ip-meta
  [obj]
  (let [m (meta obj)]
     (str "line " (:instaparse.gll/start-line m) ", column " (:instaparse.gll/start-column m))))

; types

(defn with-type
  [obj type]
  (with-meta obj (assoc (meta obj) "_type" type)))

(defn get-type
  [obj]
  (second (find (meta obj) "_type")))

(defn print-type
  [type]
  (match type
    [:void] "void"
    [:int] "int"
    [:string] "string"
    [:bool] "bool"
    [:atype ident] (str (print-type ident) "[]")
    [:tident [:ident a]] a
    [:ident a] a))

(m/defmonad phase-m
  "Monad describing a temporary result of compilation.
   Phase is successful iff the value stored is [:succ *]
   otherwise the value is [:err *]"
  [m-zero [:succ nil]
   m-result (fn m-result-phase [r]
              [:succ r])
   m-bind (fn m-bind-phase
            [pv f]
            (match pv
               [:succ val] (f val)
               [:err msg] pv))])

(defn err [msg] [:err msg])
(defn succ [data] [:succ data])

(defn toposort-hlp-red
  [elem-to-dep deps]
  (fn
    [buffer x]
    (let [dep (elem-to-dep x)]
      (if (or (nil? dep) (contains? deps dep))
        [(conj (first buffer) x) (second buffer)]
        [(first buffer) (conj (second buffer) x)]))))

(defn toposort
  [elems elem-to-id elem-to-dep]
  (loop
    [elms elems
     deps (hash-set)
     output []]
    (if (zero? (count elms))
      (succ output)
      (let [[to-add not-to-add] (reduce (toposort-hlp-red elem-to-dep deps) [[] []] elms)
            ids (map elem-to-id to-add)]
        (if (= not-to-add elms)
          (err "invalid class hierarchy")
          (recur
            not-to-add (reduce conj deps ids) (reduce conj output to-add)
            ))))))

(defn returns?
  [code]
  (match (first code)
    :block (some returns? (rest code))
    :vret true
    :ret true
    :cond (match code
            [_ [:elittrue] b] (recur b)
            :else false)
    :condelse (match code
                [_ [:elittrue] b1 _] (recur b1)
                [_ [:elitfalse] _ b2] (recur b2)
                [_ _ b1 b2] (and (returns? b1) (returns? b2))
                :else false)
    :while (match code
             [_ [:elittrue] _] true
             [_ [:elitfalse] _] false
             [_ _ b] (recur b))
    :else (= code [:sexp [:eapp [:ident "error"]]])))
