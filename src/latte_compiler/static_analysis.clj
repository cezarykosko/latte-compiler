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
(defrecord GlobState [classes funs std-types])

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
    (hash-set :void :int :string :boolean)
    ))

(defn vars-map
  []
  (list (hash-map)))

(defn add-var
  [map var type]
  (let
    [curr-scope (peek map)
     new-scope (conj curr-scope [var type])
     ]
    (conj (pop map) new-scope)
    )
  )

(defn lookup-var
  [map var]
  (if (nil? map)
    nil
    (let
      [type (find (peek map) var)]
      (if (nil? type)
        (lookup-var (pop map) var)
        type
        )
      )
    )
  )

(defn new-scope
  [map]
  (conj map (hash-map)))

(defn rm-scope
  [map]
  (pop map))

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
                     ;(util/println-ip-meta clssexpr)
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
  [fun]
  (match/match fun
               [_ type [:ident name] argz _]
               (let [fundef (->FunDef name (map-type type) (map map-arg (rest argz)))]
                 [name (with-meta fundef (meta fun))]))
  )

(require '[clojure.algo.monads :as m]
         '[latte-compiler.util :as util])

(defn conj-hlp
  [coll elem]
  (match/match elem
               [name fndef] (if (contains? coll name)
                              (util/err (str "function " name " defined more than once in: " (util/ip-meta fndef)))
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
               res (reduce m-conj (m-result coll) (map util/succ nfuns))]
              res)))

(defn bucketize
  [buffer expr]
  (if (= :clssdef (first expr))
    [(conj (first buffer) (second expr)) (second buffer)]
    [(first buffer) (conj (second buffer) expr)]
    )
  )

(defn main-check
  [glob-state]
  (if (=
        (second (find (.-funs glob-state) "main"))
        (->FunDef "main" :int []))
    (util/succ glob-state)
    (util/err "no correct main function found")
    )
  )

(defn analyze-class
  [glob-state clssexpr]
  ;(println clssexpr)
  (util/succ (match/match (first clssexpr)
                          :noextclssdef (makeclassdef glob-state clssexpr false)
                          :extclssdef (makeclassdef glob-state clssexpr true))))

(defn add-arg
  [vmap arg]
  (match/match arg [_ type [_ ident]]
               (add-var vmap ident type)
               )
  )

(defn add-args
  [vmap args]
  (loop
    [vmap vmap
     args args]
    (if (nil? args)
      vmap
      (recur (add-arg vmap (first args)) (rest args))))
  )

(defn analyze-fun
  [glob-state funexpr]
  (match/match funexpr [_ [type] _ args block]
               (let
                 [vars (add-args (vars-map) (rest args))]

                 )

               )
  (util/succ glob-state)
  )

(defn check
  [glob-state expr]
  (println expr)
  (m/domonad util/phase-m
             [state glob-state
              res (match/match (first expr)
                               ;:clssdef (analyze-class state (second expr))
                               :fndef (analyze-fun state expr))]
             (do (println res) res))
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
                new-glob-state (m-result (update glob-state :funs (fn [_] funs)))
                n-glob-state (do (println new-glob-state) (main-check new-glob-state))
                result (do (println n-glob-state) (reduce check (m-result n-glob-state) (vec tree)))
                ]
               (do
                 result)
               )

    ))

(analize (vec [
               [:fndef [:int] [:ident "main"] [:args] [:block [:sexp [:expr [:eapp [:ident "printInt"] [:expr [:elitint 1]]]]] [:sexp [:expr [:evar [:ident "return"]]]]]]
               [:fndef [:int] [:ident "g"] [:args [:arg [:tident [:ident "string"]] [:ident "a"]]] [:block [:ret [:expr [:eadd [:elitint 4] [:plus] [:elitint 2]]]]]]
               [:fndef [:int] [:ident "h"] [:args [:arg [:tident [:ident "string"]] [:ident "a"]]] [:block [:ret [:expr [:eadd [:elitint 4] [:plus] [:elitint 2]]]]]]
               [:fndef [:int] [:ident "f"] [:args [:arg [:int] [:ident "a"]] [:arg [:int] [:ident "b"]]] [:block [:ret [:expr [:eapp [:ident "g"] [:expr [:estring "132"]]]]]]]
               ]))
