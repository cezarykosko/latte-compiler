(ns latte-compiler.core
  (:require
    [latte-compiler.util :as util]
    [latte-compiler.grammar :as grammar]
    [latte-compiler.static-analysis :as analysis]
    [latte-compiler.compilation :as compilation]
    )
  (:import
           (latte_compiler.util CompilationPhase))
  )

(defn -main
  [filepath]
  (->> filepath slurp grammar/parse
       (util/apply-phase analysis/analize)
       ;#(do
       ;  (println %)
       ;  (util/apply-phase analysis/analize %))
       ;(util/apply-phase compilation/asm-compile)
       ;#(do
       ;  (println 'xddd')
         (util/apply-phase (fn [_] (util/println-err "OK"))))
       )

