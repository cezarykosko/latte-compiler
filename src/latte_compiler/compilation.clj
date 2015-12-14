(ns latte-compiler.compilation
  (:require [latte-compiler.util :as util]))

(deftype CompilationResult [output]
  util/CompilationPhase
  (successful [this]
    true)
  (output [this]
    nil)
  )
(defn asm-compile
  [tree]
  (->CompilationResult tree))
