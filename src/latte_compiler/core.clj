(ns latte-compiler.core
  (:require
    [clojure.algo.monads :as m]
    [clojure.core.match :as match]
    [latte-compiler.util :as util]
    [latte-compiler.grammar :as grammar]
    [latte-compiler.static-analysis :as analysis]
    [latte-compiler.compilation :as compilation]
    )
  (:import
           (latte_compiler.util CompilationPhase))
  )

(defn run
  [filepath]

  (m/domonad util/phase-m
             [code (m-result (slurp filepath))
              tree (grammar/parse code)
              ]

             tree))

(defn -main
  [filepath]
  (match/match (run filepath)
               [:succ _] (util/println-err "OK")
               [:err msg] (util/println-err msg))
  ;(let
  ;  [output (run filepath)])
  ;(->> filepath
  ;     slurp
  ;     grammar/parse
       ;(util/apply-phase analysis/analize)
       ;#(do
       ;  (println %)
       ;  (util/apply-phase analysis/analize %))
       ;(util/apply-phase compilation/asm-compile)
       ;#(do
       ;  (println 'xddd')
       ;  (util/apply-phase (fn [output] (println output) (util/println-err "OK"))))
       )

