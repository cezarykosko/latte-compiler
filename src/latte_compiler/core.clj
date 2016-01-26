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
  (:import (java.io FileNotFoundException)))

(defn- read-file
  [filepath]
  (try
    (util/succ (slurp filepath))
    (catch FileNotFoundException _ (util/err (str "file " filepath " not found")))
    ))

(defn- run
  [filepath]

  (m/domonad util/phase-m
    [code (read-file filepath)
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
