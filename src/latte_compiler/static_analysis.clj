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
  (successful [this] (not (:violations (.-glob-state this))))
  (output [this] (.-input this)))

(def default-state
  (->GlobState
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
    ))


(defn makeclassdefmap
  [glob-state startmap name classdefs]
  glob-state)

(defn makeclassdef
  [glob-state clssexpr extends]
  ;(println clssexpr)
  (match/match [extends (nth clssexpr 2)]
               [true [:tident [:ident ident]]]
               (let [parent-map (get (:classes glob-state) ident)]
                 (if (nil? parent-map)
                   (do
                     ;(util/println-err (str "ERROR: no such type: " ident " in"))
                     ;(util/println-ip-meta clssexpr)
                     (assoc glob-state :violations true)
                     )
                   (makeclassdefmap glob-state parent-map (second clssexpr) (nth clssexpr 3)))
                 )
               [false _] (makeclassdefmap glob-state (hash-map) (second clssexpr) (nth clssexpr 2))
               )
  )

(defn analyze-class
  [glob-state clssexpr]
  ;(println clssexpr)
  (match/match (first clssexpr)
               :noextclssdef (makeclassdef glob-state clssexpr false)
               :extclssdef (makeclassdef glob-state clssexpr true)))

(defn check
  [glob-state expr]
  ;(println expr)
  (match/match (first expr)
               :clssdef (analyze-class glob-state (second expr))
               :fndef glob-state)
  )

(defn class-dep
  [clsdef]
  (let [type (first clsdef)]
    (if (= type :noextclssdef)
      nil
      (second (nth clsdef 2))
      )
    ))

(defn class-name
  [clsdef]
  (second clsdef))


(defn map-type
  [type]
  (match/match (first type)
               :ident type
               :tident (second type)
               :else (first type))
  )

(defn map-arg
  [arg]
  (map-type (second arg))
  )

(defn map-fun
  [[_ type [:ident name] argz _]]
  [name (->FunDef name (map-type type) (map map-arg (rest argz)))]
  )

(defn funsred
  [funs]
  (fn [coll]
    ;(println funs)
    (reduce conj coll (map map-fun funs))))

(defn bucketize
  [buffer expr]
  (if (= :clssdef (first expr))
    [(conj (first buffer) (second expr)) (second buffer)]
    [(first buffer) (conj (second buffer) expr)]
    )
  )

(defn analize
  [tree]
  ;(reduce + (map println tree))
  (let
    [glob-state default-state
     [split-clss split-funs] (reduce bucketize [[] []] tree)
     classes (do (util/toposort (vec split-clss) class-name class-dep))
     new-glob-state (update glob-state :funs (funsred split-funs))
     result (reduce check new-glob-state (vec tree))
     ]
    (println "")
    (println "")
    (println split-funs)
    (println "")
    (println "")
    (println classes)
    (println (.-classes result))
    (->AnalysisResult tree result)))
