(ns latte-compiler.util
  (:require [clojure.algo.monads :as m]
            [clojure.core.match :as match]))

(defn third
  [coll]
  (first (next (next coll))))

(defn fourth
  [coll]
  (first (next (next (next coll)))))

(defn println-err
  [msg]
  (binding [*out* *err*]
    (println msg)
    ))

(defn ip-meta
  [obj]
  (let [m (meta obj)]
     (str "line " (:instaparse.gll/start-line m) ", column " (:instaparse.gll/start-column m))
      ))

(m/defmonad phase-m
  "Monad describing a temporary result of compilation.
   Phase is successful iff the value stored is [:succ *]
   otherwise the value is [:err *]"
  [m-zero [:succ nil]
   m-result (fn m-result-phase [r]
              [:succ r])
   m-bind (fn m-bind-phase
            [pv f]
            (match/match pv
               [:succ val] (f val)
               [:err msg] pv)
            )
   ])

(defn err [msg] [:err msg])
(defn succ [data] [:succ data])

(m/domonad phase-m
           [a (m-result 13)
            b [:err 44]]
            b)

(defn toposort-hlp-red
  [elem-to-dep deps]
  (fn
    [buffer x]
    (let [dep (elem-to-dep x)]
      (if (or (nil? dep) (contains? deps dep))
        [(conj (first buffer) x) (second buffer)]
        [(first buffer) (conj (second buffer) x)]
        )
      )
    )
  )

(defn toposort
  [elems elem-to-id elem-to-dep]
  (loop
    [
     elms elems
     deps (hash-set)
     output []
     ]
    (if (= 0 (count elms))
      (succ output)
      (let [[to-add not-to-add] (reduce (toposort-hlp-red elem-to-dep deps) [[] []] elms)
            ids (map elem-to-id to-add)]
        (if (= not-to-add elms)
          (err "invalid class hierarchy")
          (recur
            not-to-add (reduce conj deps ids) (reduce conj output to-add)
            ))
        )
      )
    )
  )

(defn returns?
  [code]
  (match/match (first code)
    :block (some returns? (rest code))
    :vret true
    :ret true
    :cond (match/match code
            [_ [:elittrue] b] (recur b)
            :else false
            )
    :condelse (match/match code
                [_ [:elittrue] b1 _] (recur b1)
                [_ [:elitfalse] _ b2] (recur b2)
                [_ _ b1 b2] (and (returns? b1) (returns? b2))
                :else false)
    :while (match/match code
             [_ [:elittrue] _] true
             [_ [:elitfalse] _] false
             [_ _ b] (recur b))
    :else (= code [:sexp [:eapp [:ident "error"]]])
    ))
