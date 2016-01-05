(ns latte-compiler.static-analysis
  (:require [clojure.core.match :as match]
            [clojure.algo.monads :as m]
            [latte-compiler.util :as util]))

(defrecord ClassDef [extends fields funs])
(defrecord FunDef [name outType inTypes])
(defrecord FieldDef [name type])
(defrecord GlobState [classes funs std-types])

(defn third
  [coll]
  (first (next (next coll))))

(defn fourth
  [coll]
  (first (next (next (next coll)))))

(defn merge-checks
  [acc cur]
  (m/domonad util/phase-m
    [acc acc
     curr cur]
    (conj acc curr)
    ))

(def default-state
  (->GlobState
    (hash-map)
    (hash-map
      [:ident "printInt"] (->FunDef "printInt" [:void] [[:int]])
      [:ident "printString"] (->FunDef "printString" [:void] [[:string]])
      [:ident "error"] (->FunDef "error" [:void] [])
      [:ident "readInt"] (->FunDef "readInt" [:int] [])
      [:ident "readString"] (->FunDef "readString" [:string] [])
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

(defn print-var
  [var]
  (match/match var
    [:ident name] (str name)
    :else "dsdsa"))

(defn lookup-var
  [map var location]
  (if (empty? map)
    (do (println "starem") (util/err (str "var " (print-var var) " not found in: " location)))
    (let
      [type (find (peek map) var)]
      (if (nil? type)
        (recur (pop map) var location)
        (util/succ (second type))
        )
      )
    )
  )

(defn new-scope
  [map]
  (println map)
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
    ;:tident (second type)
    :else type)
  )

(defn map-arg
  [arg]
  (map-type (second arg))
  )

(defn map-fun
  [fun]
  (match/match fun
    [_ type name argz _]
    (let [fundef (->FunDef name (map-type type) (map map-arg (rest argz)))]
      [name (with-meta fundef (meta fun))]))
  )

(require '[clojure.algo.monads :as m]
  '[latte-compiler.util :as util])

(defn conj-hlp
  [coll elem]
  (match/match elem
    [name fndef] (if (contains? coll name)
                   (util/err (str "function " (second name) " defined more than once in: " (util/ip-meta fndef)))
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
        (second (find (.-funs glob-state) [:ident "main"]))
        (->FunDef [:ident "main"] [:int] []))
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
  (match/match arg [_ type ident]
    (add-var vmap ident type)
    )
  )

(defn add-args
  [vmap args]
  (loop
    [vmap vmap
     args (rest args)]
    (if (empty? args)
      vmap
      (recur (add-arg vmap (first args)) (rest args))))
  )

(defn terminated
  [code]
  (match/match (first code)
    :block (some terminated (rest code))
    :vret true
    :ret true
    :cond (match/match code
            [_ [:elittrue] b] (recur b)
            :else false
            )
    :condelse (match/match code
                [_ [:elittrue] b1 _] (recur b1)
                [_ [:elitfalse] _ b2] (recur b2)
                :else false)
    :while (match/match code
             [_ [:elittrue] _] true
             [_ [:elitfalse] _] false
             [_ _ b] (recur b))
    :else (= code [:sexp [:eapp [:ident "error"]]])
    ))

(defn print-type
  [type]
  (match/match type
    [:void] "void"
    [:int] "int"
    [:string] "string"
    [:bool] "bool"
    [:tident [:ident a]] a))

(defn check-types
  [exp-type actual-type res location]
  (m/domonad util/phase-m
    [exp-type exp-type
     actual-type (do (println actual-type) actual-type)
     res (do (println "AA") (println exp-type) (println actual-type) (if (= actual-type exp-type)
                                                                       (util/succ res)
                                                                       (util/err (str "return type invalid; expected " (print-type exp-type) ", found " (print-type actual-type) " in " location))))
     ]
    res)

  )

(defn with-type
  [obj type]
  (with-meta obj (assoc (meta obj) "_type" type))
  )

(defn get-type
  [obj]
  (second (find (meta obj) "_type")))

(defn annotate-expr
  [glob-state vars expr]
  (let [location (util/ip-meta expr)]
    (match/match (first expr)
      :elitint (util/succ (with-type expr [:int]))
      :elittrue (util/succ (with-type expr [:bool]))
      :elitfalse (util/succ (with-type expr [:bool]))
      :estring (util/succ (with-type expr [:string]))
      :evar (m/domonad util/phase-m
              [type (lookup-var vars (second expr) (util/ip-meta expr))]
              (with-type expr type))
      :neg (m/domonad util/phase-m
             [inside-expr (annotate-expr glob-state vars (second expr))
              res (if (= (get-type inside-expr) [:int])
                    (util/succ (with-type [:neg inside-expr] [:int]))
                    (util/err (str "expected int, found " (print-type (get-type inside-expr)) " in: " location))
                    )]
             res
             )
      :not (m/domonad util/phase-m
             [inside-expr (annotate-expr glob-state vars (second expr))
              res (if (= (get-type inside-expr) [:bool])
                    (util/succ (with-type [:not inside-expr] [:bool]))
                    (util/err (str "expected bool, found " (print-type (get-type inside-expr)) " in: " location))
                    )]
             res
             )
      :eor (m/domonad util/phase-m
             [lexpr (annotate-expr glob-state vars (second expr))
              rexpr (annotate-expr glob-state vars (third expr))
              res (if (and
                        (= (get-type lexpr) [:bool])
                        (= (get-type rexpr) [:bool]))
                    (util/succ (with-type [:eor lexpr rexpr] [:bool]))
                    )
              ]
             res)
      :eand (m/domonad util/phase-m
              [lexpr (annotate-expr glob-state vars (second expr))
               rexpr (annotate-expr glob-state vars (third expr))
               res (if (and
                         (= (get-type lexpr) [:bool])
                         (= (get-type rexpr) [:bool]))
                     (util/succ (with-type [:eand lexpr rexpr] [:bool]))
                     (util/err (str "expected bool, bool, found " (print-type (get-type lexpr)) ", " (print-type (get-type rexpr)) " in: " location))
                     )
               ]
              res)
      :erel (m/domonad util/phase-m
              [lexpr (annotate-expr glob-state vars (second expr))
               rexpr (annotate-expr glob-state vars (fourth expr))
               op (m-result (third expr))
               res (if (and
                         (= (get-type lexpr) [:int])
                         (= (get-type rexpr) [:int]))
                     (util/succ (with-type [:erel lexpr op rexpr] [:bool]))
                     (util/err (str "expected int,int, found " (print-type (get-type lexpr)) ", " (print-type (get-type rexpr)) " in: " location))
                     )
               ]
              res)
      :emul (m/domonad util/phase-m
              [lexpr (annotate-expr glob-state vars (second expr))
               rexpr (annotate-expr glob-state vars (fourth expr))
               op (m-result (third expr))
               res (if (and
                         (= (get-type lexpr) [:int])
                         (= (get-type rexpr) [:int]))
                     (util/succ (with-type [:emul lexpr op rexpr] [:int]))
                     (util/err (str "expected int,int, found " (print-type (get-type lexpr)) ", " (print-type (get-type rexpr)) " in: " location))
                     )
               ]
              res)
      :eadd (match/match (third expr)
              [:minus]
              (m/domonad util/phase-m
                [lexpr (annotate-expr glob-state vars (second expr))
                 rexpr (annotate-expr glob-state vars (fourth expr))
                 res (if (and
                           (= (get-type lexpr) [:int])
                           (= (get-type rexpr) [:int]))
                       (util/succ (with-type [:eadd lexpr [:minus] rexpr] [:int]))
                       (util/err (str "expected int,int, found " (print-type (get-type lexpr)) ", " (print-type (get-type rexpr)) " in: " location))
                       )
                 ]
                res)
              [:plus]
              (m/domonad util/phase-m
                [lexpr (annotate-expr glob-state vars (second expr))
                 rexpr (annotate-expr glob-state vars (fourth expr))
                 res (if (or
                           (and
                             (= (get-type lexpr) [:int])
                             (= (get-type rexpr) [:int]))
                           (and
                             (= (get-type lexpr) [:string])
                             (= (get-type rexpr) [:string])))
                       (util/succ (with-type [:eadd lexpr [:plus] rexpr] (get-type lexpr)))
                       (util/err (str "expected int,int or string,string, found " (print-type (get-type lexpr)) ", " (print-type (get-type rexpr)) " in: " location))
                       )
                 ]
                res)
              )
      :eapp (m/domonad util/phase-m
              [ident (m-result (second expr))
               args (reduce merge-checks (m-result []) (map #(annotate-expr glob-state vars %) (rest (rest expr))))
               actargtypes (m-result (vec (map get-type args)))
               fundef (m-result (second (find (.-funs glob-state) ident)))
               expargtypes (m-result (.-inTypes fundef))
               outtype (m-result (.-outType fundef))
               res (if (= expargtypes actargtypes)
                     (util/succ (with-type [:eapp ident args] outtype))
                     (do (println ident) (println expargtypes) (println args) (println actargtypes) (util/err (str "somthieng in " location)))
                     )
               ]
              res
              )
      ))
  )

(defn annotate-code
  [glob-state vars code]
  (let [location (util/ip-meta code)]
    (match/match (first code)
      :block (m/domonad util/phase-m
               [[_ nres] (reduce (fn [env code]
                                   (m/domonad util/phase-m
                                     [[vars blk] env
                                      [nvars res] (annotate-code glob-state vars code)]
                                     [nvars (conj blk res)]
                                     )
                                   ) (m-result [(new-scope vars) [:block]]) (rest code))]
               nres
               )
      :vret (check-types (util/succ [:void]) (lookup-var vars "_return_" location) [vars [:vret]] location)
      :ret (m/domonad util/phase-m
             [expr (annotate-expr glob-state vars (second code))
              res (check-types (lookup-var vars "_return_" location) (m-result (get-type expr)) [vars (with-type [:ret expr] (get-type expr))] location)
              ]
             res)
      :incr (check-types (util/succ [:int]) (lookup-var vars (second code) location) [vars (with-type code [:int])] location)
      :decr (check-types (util/succ [:int]) (lookup-var vars (second code) location) [vars (with-type code [:int])] location)
      :decl (let
              [
               type (second code)
               decls (rest (rest code))
               ]
              (util/succ [(reduce #(add-var %1 %2 type) vars (map second decls)) (with-type code type)])
              )
      :ass (m/domonad util/phase-m
             [name (m-result (second code))
              expr (annotate-expr glob-state vars (third code))
              res (check-types (lookup-var vars name location) (m-result (get-type expr)) [vars (with-type [:ass name expr] (get-type expr))] location)]
             res)
      :cond (m/domonad util/phase-m
              [[_ nblock] (annotate-code glob-state vars (third code))
               expr (annotate-expr glob-state vars (second code))
               res (check-types (m-result [:bool]) (m-result (get-type expr)) [vars [:cond expr nblock]] location)]
              res)
      :condelse (m/domonad util/phase-m
                  [[_ nblock1] (annotate-code glob-state vars (third code))
                   [_ nblock2] (annotate-code glob-state vars (fourth code))
                   expr (annotate-expr glob-state vars (second code))
                   res (check-types (m-result [:bool]) (m-result (get-type expr)) [vars [:condelse expr nblock1 nblock2]] location)]
                  res)
      :while (m/domonad util/phase-m
               [[_ nblock] (annotate-code glob-state vars (third code))
                expr (annotate-expr glob-state vars (second code))
                res (check-types (m-result [:bool]) (m-result (get-type expr)) [vars [:while expr nblock]] location)]
               [vars res])
      :sexp (m/domonad util/phase-m
              [expr (annotate-expr glob-state vars (second code))]
              [vars [:sexp expr]]
              )
      :else (util/succ [vars code]))))

(defn check-type
  [glob-state funexpr]
  (match/match funexpr [fun get-type [ident name] args block]
    (m/domonad util/phase-m
      [
       vars (m-result (add-args (add-var (vars-map) "_return_" get-type) args))
       nblock (annotate-code glob-state vars block)
       ]
      [fun get-type [ident name] args nblock]
      )))

(defn analyze-fun
  [glob-state funexpr]
  (match/match funexpr [_ [get-type] [_ name] args block]
    (if (not (or (= get-type :void) (terminated block)))
      (do (println "err") (util/err (str "return not found in function " name "\n" (util/ip-meta funexpr))))
      (do (println funexpr) (check-type glob-state funexpr))
      )
    )
  )

(defn check
  [glob-state]
  (println glob-state)
  (fn [expr]
    (m/domonad util/phase-m
      [state glob-state
       res (match/match (first expr)
             ;:clssdef (analyze-class state (second expr))
             :fndef (analyze-fun state expr))]
      (do (println "ser") (println res) res))))



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
       n-glob-state (do (println "A") (main-check new-glob-state))
       result (do (println "B") (reduce merge-checks (m-result []) (map (check (m-result n-glob-state)) tree)))
       ]
      result
      )))

(println (analize (vec [
                        [:fndef [:int] [:ident "main"] [:args] [:block [:sexp [:eapp [:ident "printInt"] [:elitint 1]]] [:sexp [:eapp [:ident "error"]]] [:condelse [:elittrue] [:block] [:block [:ret [:elitint 0]]]] [:while [:erel [:evar [:ident "a"]] [:eq] [:elitint 3]] [:block [:ret [:elitint 0]]]] [:ret [:elitint 132]]]]
                        ])))
;
(println (analize (vec [
                        [:fndef [:int] [:ident "main"] [:args] [:block [:incr [:ident "a"]] [:ret [:estring "abc"]]]]])))

;(analize (vec [
;               [:fndef [:int] [:ident "main"] [:args] [:block [:sexp [:expr [:eapp [:ident "printInt"] [:expr [:elitint 1]]]]] [:vret]]]
;               [:fndef [:int] [:ident "g"] [:args [:arg [:tident [:ident "string"]] [:ident "a"]]] [:block [:ret [:expr [:eadd [:elitint 4] [:plus] [:elitint 2]]]]]]
;               [:fndef [:int] [:ident "h"] [:args [:arg [:tident [:ident "string"]] [:ident "a"]]] [:block [:ret [:expr [:eadd [:elitint 4] [:plus] [:elitint 2]]]]]]
;               [:fndef [:int] [:ident "f"] [:args [:arg [:int] [:ident "a"]] [:arg [:int] [:ident "b"]]] [:block [:ret [:expr [:eapp [:ident "g"] [:expr [:estring "132"]]]]]]]
;               ]))
