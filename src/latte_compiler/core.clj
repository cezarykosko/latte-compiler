(ns latte-compiler.core
  (:require
    [latte-compiler.grammar :as grammar]
    [latte-compiler.static-analysis :as analysis]
    [latte-compiler.compilation :as compilation]
    [latte-compiler.util :as util]))

(defn -main
  [filepath]
  (->> filepath slurp grammar/parse
       #(do
         (println %)
         (util/apply-phase analysis/analize %))
       (util/apply-phase compilation/asm-compile)
       (util/apply-phase (fn [_] (util/println-err "OK"))))
  )
