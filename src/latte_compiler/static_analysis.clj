(ns latte-compiler.static-analysis
  (:require [clojure.core.match :as match]
            [latte-compiler.util :as util]))

;kryteria
; -> nazwy
; -> typy
; -> jest main
; -> returny

(defrecord ClassDef [extends fields funs])
(defrecord FunDef [name outType inTypes])
(defrecord FieldDef [name type])
(defrecord GlobState [classes funs violations std-types])

(deftype AnalysisResult [input glob-state]
  util/CompilationPhase
  (successful [this] (:violations this))
  (output [this] (:input this)))

(defn makeclassdefmap
  [glob-state startmap name classdefs]
  glob-state)

(defn makeclassdef
  [glob-state clssexpr extends]
  (match/match [extends (nth clssexpr 2)]
               [true [:tident [:ident ident]]]
               (let [parent-map (get (:classes glob-state) ident)]
                 (if (nil? parent-map)
                   (do
                     (util/println-err (str "ERROR: no such type: " ident " in"))
                     (util/println-ip-meta clssexpr)
                     (assoc-in glob-state :violations true)
                     )
                   (makeclassdefmap glob-state parent-map (second clssexpr) (nth clssexpr 3)))
                 )
               [false _] (makeclassdefmap glob-state (hash-map) (second clssexpr) (nth clssexpr 2))
               )
  )

(defn analyze-class
  [glob-state clssexpr]
  (match/match (first clssexpr)
               :noextclssdef (makeclassdef glob-state clssexpr false)
               :extclssdef (makeclassdef glob-state clssexpr true)))

(defn check
  [glob-state expr]
  (println expr)
  (match/match (first expr)
               :clssdef (analyze-class glob-state (second expr))
               :fndef glob-state)
  )

(defn analize
  [tree]
  (let
    [glob-state (->GlobState
                  (hash-map)
                  (hash-map
                    "printInt" (->FunDef "printInt" :void [:int])
                    "printString" (->FunDef "printString" :void [:string])
                    "error" (->FunDef "error" :void [])
                    "readInt" (->FunDef "readInt" :int [])
                    "readString" (->FunDef "readString" :string [])
                    )
                  false
                  (hash-set :void :int :string :boolean)
                  )
     result (reduce check glob-state (vec tree))]
    (->AnalysisResult tree result)))
