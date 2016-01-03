(ns latte-compiler.static-analysis
  (:require [clojure.core.match :as match]
            [clojure.algo.monads :as m]
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
                     (util/println-err (str "ERROR: no such type: " ident " in"))
                     (util/println-ip-meta clssexpr)
                     (assoc glob-state :violations true)
                     )
                   (makeclassdefmap glob-state parent-map (second clssexpr) (nth clssexpr 3)))
                 )
               [false _] (makeclassdefmap glob-state (hash-map) (second clssexpr) (nth clssexpr 2))
               )
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

(require '[clojure.algo.monads :as m]
         '[latte-compiler.util :as util])

(defn conj-hlp
  [coll elem]
  (match/match elem
               [name _] (if (contains? coll name)
                          (util/err (concat "function " name " already defined in :" (util/ip-meta elem)))
                          (util/succ (conj coll elem))
                          ))
  )

(defn m-conj
  [mcoll melem]
  (m/domonad util/phase-m
             [
              coll mcoll
              elem melem
              res (conj-hlp coll elem)]
             res))



(defn funsred
  ([funs coll]
   (m/domonad util/phase-m
              [mfuns (m-result funs)
               nfuns (util/succ (map map-fun funs))
               res  (reduce m-conj (m-result coll) (map util/succ nfuns))]
              res)))


;(funsred (vec [
;               [:fndef [:void] [:ident "main"] [:args] [:block [:sexp [:expr [:eapp [:ident "printInt"] [:expr [:elitint 1]]]]] [:sexp [:expr [:evar [:ident "return"]]]]]]
;               [:fndef [:int] [:ident "g"] [:args [:arg [:tident [:ident "string"]] [:ident "a"]]] [:block [:ret [:expr [:eadd [:elitint 4] [:plus] [:elitint 2]]]]]]
;               [:fndef [:int] [:ident "f"] [:args [:arg [:int] [:ident "a"]] [:arg [:int] [:ident "b"]]] [:block [:ret [:expr [:eapp [:ident "g"] [:expr [:estring "132"]]]]]]]
;               ])
;         (hash-map
;           "printInt" (->FunDef "printInt" :void [:int])
;           "printString" (->FunDef "printString" :void [:string])
;           "error" (->FunDef "error" :void [])
;           "readInt" (->FunDef "readInt" :int [])
;           "readString" (->FunDef "readString" :string [])
;           ))


(defn bucketize
  [buffer expr]
  (if (= :clssdef (first expr))
    [(conj (first buffer) (second expr)) (second buffer)]
    [(first buffer) (conj (second buffer) expr)]
    )
  )

(defn analyze-class
  [glob-state clssexpr]
  ;(println clssexpr)
  (util/succ (match/match (first clssexpr)
                          :noextclssdef (makeclassdef glob-state clssexpr false)
                          :extclssdef (makeclassdef glob-state clssexpr true))))

(defn analyze-fun
  [glob-state funexpr]
  (util/succ glob-state)
  )

(defn check
  [glob-state expr]
  (println expr)
  (m/domonad util/phase-m
             [state (m-result glob-state)
              res (match/match (first expr)
                               ;:clssdef (analyze-class state (second expr))
                               :fndef (analyze-fun state expr))]
             res)
  )

(defn analize
  [tree]
  (let
    [glob-state default-state
     [split-clss split-funs] (reduce bucketize [[] []] tree)
     ;classes (do (util/toposort (vec split-clss) class-name class-dep))
     ]
    (m/domonad util/phase-m
               [funs (funsred split-funs (.-funs glob-state))
                new-glob-state (do (println "A") (println funs) (m-result (update glob-state :funs funs)))
                result (do (println new-glob-state) (reduce check new-glob-state (vec tree)))
                ]
               (do
                 result)
               )

    ))

(analize (vec [
               [:fndef [:void] [:ident "main"] [:args] [:block [:sexp [:expr [:eapp [:ident "printInt"] [:expr [:elitint 1]]]]] [:sexp [:expr [:evar [:ident "return"]]]]]]
               [:fndef [:int] [:ident "g"] [:args [:arg [:tident [:ident "string"]] [:ident "a"]]] [:block [:ret [:expr [:eadd [:elitint 4] [:plus] [:elitint 2]]]]]]
               [:fndef [:int] [:ident "g"] [:args [:arg [:tident [:ident "string"]] [:ident "a"]]] [:block [:ret [:expr [:eadd [:elitint 4] [:plus] [:elitint 2]]]]]]
               [:fndef [:int] [:ident "f"] [:args [:arg [:int] [:ident "a"]] [:arg [:int] [:ident "b"]]] [:block [:ret [:expr [:eapp [:ident "g"] [:expr [:estring "132"]]]]]]]
               ]))
