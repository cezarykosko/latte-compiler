(ns latte-compiler.core
  (:require
    [clojure.algo.monads :as m]
    [clojure.core.match :as match]
    [latte-compiler.util :as util]
    [latte-compiler.grammar :as grammar]
    [latte-compiler.static-analysis :as analysis]
    [latte-compiler.compilation :as compilation]
    )
  (:gen-class)
  )

(defn- run
  [filepath]

  (m/domonad util/phase-m
    [code (m-result (slurp filepath))
     tree (grammar/parse code)
     aug-tree (analysis/analize tree)
     ]
    aug-tree))

(defn -main
  [filepath]
  (match/match (run filepath)
    [:succ tree] (do
                   (util/println-err "OK")
                   (compilation/asm-compile tree)
                   )
    [:err msg] (do
                 (util/println-err "ERROR")
                 (util/println-err msg)
                 (System/exit 1)
                 ))
  )

