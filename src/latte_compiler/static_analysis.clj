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
      [:ident "printInt"] (->FunDef [:ident "printInt"] [:void] [[:int]])
      [:ident "printString"] (->FunDef [:ident "printString"] [:void] [[:string]])
      [:ident "error"] (->FunDef [:ident "error"] [:void] [])
      [:ident "readInt"] (->FunDef [:ident "readInt"] [:int] [])
      [:ident "readString"] (->FunDef [:ident "readString"] [:string] [])
      )
    (hash-set :void :int :string :boolean)
    ))

(defn print-var
  [var]
  (match/match var
    [:ident name] (str name)))

(defn vars-map
  []
  [(list (hash-map)) 0 0 (hash-map)])

(defn add-var
  [[map num-vars num-strings strings] var type]
  (let
    [curr-scope (peek map)
     new-scope (conj curr-scope [var [type (- (+ 1 num-vars))]])
     ]
    (if (contains? curr-scope var)
      (util/err (str "var " (print-var var) " already declared in " (util/ip-meta var)))
      (util/succ [(- (+ 1 num-vars)) [(conj (pop map) new-scope) (+ num-vars 1) num-strings strings]]))
    )
  )

(defn add-string
  [[map num-vars num-strings strings] string]
  (if (contains? strings string)
    [(second (find strings string)) [map num-vars num-strings strings]]
    [(+ num-strings 1) [map num-vars (+ num-strings 1) (assoc (assoc strings string (+ num-strings 1)) (+ num-strings 1) string)]]
    )
  )

(defn add-arg-var
  [[map num-vars num-strings strings] var type]
  (let
    [curr-scope (peek map)
     new-scope (conj curr-scope [var [type (+ 1 num-vars)]])
     ]
    (if (contains? curr-scope var)
      (util/err (str "var " (print-var var) " already declared in " (util/ip-meta var)))
      (util/succ [(+ 1 num-vars) [(conj (pop map) new-scope) (+ num-vars 1) num-strings strings]]))
    )
  )

(defn lookup-var
  [[map a b c] var location]
  (if (empty? map)
    (util/err (str "var " (print-var var) " not found in: " location))
    (let
      [type (find (peek map) var)]
      (if (nil? type)
        (recur [(pop map) a b c] var location)
        (util/succ (second type))
        )
      )
    )
  )

(defn lookup-fun
  [glob-state ident location]
  (let
    [funs (.-funs glob-state)
     rec (find funs ident)]
    (if (nil? rec)
      (util/err (str "function " (print-var ident) " not found in: " location))
      (util/succ (second rec))
      )))

(defn new-scope
  [[map num-vars num-strings strings]]
  [(conj map (hash-map)) num-vars num-strings strings])

(defn rm-scope
  [[map num-vars num-strings strings]]
  [(pop map) num-vars num-strings strings])

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
  (util/succ (match/match (first clssexpr)
               :noextclssdef (makeclassdef glob-state clssexpr false)
               :extclssdef (makeclassdef glob-state clssexpr true))))

(defn add-arg
  [vmap arg]
  (match/match arg [:arg type ident]
    (m/domonad util/phase-m
      [[num vmap2] (add-arg-var vmap ident type)]
      [[:arg type [:ident num]] vmap2]
      )
    )
  )

(defn add-args
  [vmap args]
  (reduce (fn [buf arg]
            (m/domonad util/phase-m
              [[tmprgs vmap2] buf
               [narg vmap3] (add-arg vmap2 arg)] [(conj tmprgs narg) vmap3]))
    [:succ [[:args] vmap]] (rest args))
  )

(defn print-type
  [type]
  (match/match type
    [:void] "void"
    [:int] "int"
    [:string] "string"
    [:bool] "bool"
    [:tident [:ident a]] a))

(defn print-args
  [args]
  (if (empty? args)
    "none"
    (clojure.string/join "," (map print-type args))
    )
  )

(defn check-types
  [exp-type actual-type res location]
  (if (= actual-type exp-type)
    (util/succ res)
    (util/err (str "return type invalid; expected " (print-type exp-type) ", found " (print-type actual-type) " in " location))))

(defn with-type
  [obj type]
  (with-meta obj (assoc (meta obj) "_type" type))
  )

(defn get-type
  [obj]
  (second (find (meta obj) "_type")))

(defn with-vars
  [obj vars]
  (with-meta obj (assoc (meta obj) "_vars" vars))
  )

(defn annotate-expr
  [glob-state vars expr]
  (let [location (util/ip-meta expr)]
    (match/match (first expr)
      :elitint (util/succ [vars (with-type expr [:int])])
      :elittrue (util/succ [vars (with-type expr [:bool])])
      :elitfalse (util/succ [vars (with-type expr [:bool])])
      :estring (let
                 [[num nvar] (add-string vars (second expr))]
                 (util/succ [nvar (with-type [:estring num] [:string])]))
      :evar (m/domonad util/phase-m
              [[type num] (lookup-var vars (second expr) (util/ip-meta expr))]
              [vars (with-type [:evar [:ident num]] type)])
      :neg (m/domonad util/phase-m
             [[nvars inside-expr] (annotate-expr glob-state vars (second expr))
              res (if (= (get-type inside-expr) [:int])
                    (util/succ [nvars (with-type [:neg inside-expr] [:int])])
                    (util/err (str "expr invalid; expected int, found " (print-type (get-type inside-expr)) " in: " location))
                    )]
             res
             )
      :not (m/domonad util/phase-m
             [[nvars inside-expr] (annotate-expr glob-state vars (second expr))
              res (if (= (get-type inside-expr) [:bool])
                    (util/succ [nvars (with-type [:not inside-expr] [:bool])])
                    (util/err (str "expr invalid; expected bool, found " (print-type (get-type inside-expr)) " in: " location))
                    )]
             res
             )
      :eor (m/domonad util/phase-m
             [[vars1 lexpr] (annotate-expr glob-state vars (second expr))
              [vars2 rexpr] (annotate-expr glob-state vars1 (third expr))
              res (if (and
                        (= (get-type lexpr) [:bool])
                        (= (get-type rexpr) [:bool]))
                    (util/succ [vars2 (with-type [:eor lexpr rexpr] [:bool])])
                    (util/err (str "expr invalid; expected bool, bool, found " (print-type (get-type lexpr)) ", " (print-type (get-type rexpr)) " in: " location))
                    )
              ]
             res)
      :eand (m/domonad util/phase-m
              [[vars1 lexpr] (annotate-expr glob-state vars (second expr))
               [vars2 rexpr] (annotate-expr glob-state vars1 (third expr))
               res (if (and
                         (= (get-type lexpr) [:bool])
                         (= (get-type rexpr) [:bool]))
                     (util/succ [vars2 (with-type [:eand lexpr rexpr] [:bool])])
                     (util/err (str "expr invalid; expected bool,bool, found " (print-type (get-type lexpr)) ", " (print-type (get-type rexpr)) " in: " location))
                     )
               ]
              res)
      :erel (m/domonad util/phase-m
              [[vars1 lexpr] (annotate-expr glob-state vars (second expr))
               [vars2 rexpr] (annotate-expr glob-state vars1 (fourth expr))
               op (m-result (third expr))
               res (if (and
                         (= (get-type lexpr) [:int])
                         (= (get-type rexpr) [:int]))
                     (util/succ [vars2 (with-type [:erel lexpr op rexpr] [:bool])])
                     (if (or
                           (= (third expr) [:eq])
                           (= (third expr) [:ieq]))

                       (if (and
                             (= (get-type lexpr) [:bool])
                             (= (get-type rexpr) [:bool])
                             (or
                               (= (third expr) [:eq])
                               (= (third expr) [:ieq])
                               )
                             )
                         (util/succ [vars2 (with-type [:erel lexpr op rexpr] [:bool])])
                         (if (and
                               (= (get-type lexpr) [:string])
                               (= (get-type rexpr) [:string])
                               (or
                                 (= (third expr) [:eq])
                                 (= (third expr) [:ieq])
                                 ))
                           (let
                             [cmp [:eapp [:ident "_eqStrings"] [lexpr rexpr]]
                              expr (if (= (third expr) [:eq])
                                     cmp
                                     [:not cmp])]
                             (util/succ [vars2 (with-type expr [:bool])]))
                           (util/err (str "expr invalid; expected int,int or bool,bool or string,string, "
                                       "found " (print-type (get-type lexpr)) ", " (print-type (get-type rexpr)) " in: " location))))
                       (util/err (str "expr invalid; expected int,int, "
                                   "found " (print-type (get-type lexpr)) ", " (print-type (get-type rexpr)) " in: " location)))
                     )
               ]
              res)
      :emul (m/domonad util/phase-m
              [[vars1 lexpr] (annotate-expr glob-state vars (second expr))
               [vars2 rexpr] (annotate-expr glob-state vars1 (fourth expr))
               op (m-result (third expr))
               res (if (and
                         (= (get-type lexpr) [:int])
                         (= (get-type rexpr) [:int]))
                     (util/succ [vars2 (with-type [:emul lexpr op rexpr] [:int])])
                     (util/err (str "expr invalid; expected int,int, found " (print-type (get-type lexpr)) ", " (print-type (get-type rexpr)) " in: " location))
                     )
               ]
              res)
      :eadd (match/match (third expr)
              [:minus]
              (m/domonad util/phase-m
                [[vars1 lexpr] (annotate-expr glob-state vars (second expr))
                 [vars2 rexpr] (annotate-expr glob-state vars1 (fourth expr))
                 res (if (and
                           (= (get-type lexpr) [:int])
                           (= (get-type rexpr) [:int]))
                       (util/succ [vars2 (with-type [:eadd lexpr [:minus] rexpr] [:int])])
                       (util/err (str "expr invalid; expected int,int, found " (print-type (get-type lexpr)) ", " (print-type (get-type rexpr)) " in: " location))
                       )
                 ]
                res)
              [:plus]
              (m/domonad util/phase-m
                [[vars1 lexpr] (annotate-expr glob-state vars (second expr))
                 [vars2 rexpr] (annotate-expr glob-state vars1 (fourth expr))
                 res (if
                       (and
                         (= (get-type lexpr) [:int])
                         (= (get-type rexpr) [:int]))
                       (util/succ [vars2 (with-type [:eadd lexpr [:plus] rexpr] [:int])])
                       (if
                         (and
                           (= (get-type lexpr) [:string])
                           (= (get-type rexpr) [:string]))
                         (util/succ [vars2 (with-type [:eapp [:ident "_concatStrings"] [lexpr rexpr]] [:string])])
                         (util/err (str "expr invalid; expected int,int or string,string, found " (print-type (get-type lexpr)) ", " (print-type (get-type rexpr)) " in: " location)))
                       )
                 ]
                res)
              )
      :eapp (m/domonad util/phase-m
              [ident (m-result (second expr))
               [nvar args] (reduce (fn [buff tmp] (m/domonad util/phase-m [[tmvar tmargs] buff [ntmvar nexpr] (annotate-expr glob-state tmvar tmp)] [ntmvar (conj tmargs nexpr)])) (m-result [vars []]) (rest (rest expr)))
               actargtypes (m-result (vec (map get-type args)))
               fundef (lookup-fun glob-state ident location)
               expargtypes (m-result (.-inTypes fundef))
               outtype (m-result (.-outType fundef))
               res (if (= expargtypes actargtypes)
                     (util/succ [nvar (with-type [:eapp ident args] outtype)])
                     (util/err (str "function invocation invalid; expected " (print-args expargtypes) " found " (print-args actargtypes) " in " location))
                     )
               ]
              res
              )
      ))
  )

(defn process-decl
  [glob-state type]
  (fn [vars decl]
    (match/match decl
      [:noinit ident] (m/domonad util/phase-m
                        [
                         [tvars decls] vars
                         [num [a b c d]] (add-var tvars ident type)
                         ]
                        [[a b c d] (conj decls [:noinit [:ident num]])]
                        )
      [:init ident expr] (m/domonad util/phase-m
                           [
                            [tvars decls] vars
                            [ntvars nexpr] (annotate-expr glob-state tvars expr)
                            ntype (m-result (get-type nexpr))
                            [num [a b c d]] (add-var ntvars ident type)
                            res (if (= type ntype)
                                  (util/succ [[a b c d] (conj decls [:init [:ident num] nexpr])])
                                  (util/err (str "declaration invalid; expected " (print-type type) ", found " (print-type ntype) " in " (util/ip-meta decl))))]
                           res
                           )
      )))

(defn annotate-code
  [glob-state vars code]
  (let [location (util/ip-meta code)]
    (match/match (first code)
      :block (m/domonad util/phase-m
               [[avars result] (reduce (fn [env code]
                                         (m/domonad util/phase-m
                                           [[vars blk] env
                                            [nvars res] (annotate-code glob-state vars code)]
                                           [nvars (conj blk res)]
                                           )
                                         ) (m-result [(new-scope vars) [:block]]) (rest code))]
               [(rm-scope avars) result]
               )
      :vret (m/domonad util/phase-m
              [[type _] (lookup-var vars "_return_" location)
               res (check-types type [:void] [vars [:vret]] location)]
              res)
      :ret (m/domonad util/phase-m
             [[nvars expr] (annotate-expr glob-state vars (second code))
              [type _] (lookup-var nvars "_return_" location)
              tmp (if (= type [:void]) (util/err (str "returning void function not allowed in " (util/ip-meta location))) (util/succ ""))
              res (check-types type (get-type expr) [nvars (with-type [:ret expr] (get-type expr))] location)
              ]
             res)
      :incr (m/domonad util/phase-m
              [[type num] (lookup-var vars (second code) location)
               res (check-types [:int] type [vars (with-type [:incr [:ident num]] [:int])] location)]
              res)
      :decr (m/domonad util/phase-m
              [[type num] (lookup-var vars (second code) location)
               res (check-types [:int] type [vars (with-type [:decr [:ident num]] [:int])] location)]
              res)
      :decl (m/domonad util/phase-m
              [type (m-result (second code))
               decls (m-result (rest (rest code)))
               [v e] (reduce (process-decl glob-state type) (m-result [vars [:decl type]]) decls)]
              [v (with-type e type)]
              )
      :ass (m/domonad util/phase-m
             [name (m-result (second code))
              [nvars expr] (annotate-expr glob-state vars (third code))
              [type num] (lookup-var vars name location)
              res (check-types type (get-type expr) [nvars (with-type [:ass [:ident num] expr] (get-type expr))] location)]
             res)
      :cond (m/domonad util/phase-m
              [
               [vars1 expr] (annotate-expr glob-state vars (second code))
               [vars2 nblock] (annotate-code glob-state vars1 (third code))
               res (check-types [:bool] (get-type expr) [vars2 [:cond expr nblock]] location)]
              res)
      :condelse (m/domonad util/phase-m
                  [
                   [vars1 expr] (annotate-expr glob-state vars (second code))
                   [vars2 nblock1] (annotate-code glob-state vars1 (third code))
                   [vars3 nblock2] (annotate-code glob-state vars2 (fourth code))
                   res (check-types [:bool] (get-type expr) [vars3 [:condelse expr nblock1 nblock2]] location)]
                  res)
      :while (m/domonad util/phase-m
               [
                [vars1 expr] (annotate-expr glob-state vars (second code))
                [vars2 nblock] (annotate-code glob-state vars1 (third code))
                res (check-types [:bool] (get-type expr) [vars2 [:while expr nblock]] location)]
               res
               )
      :sexp (m/domonad util/phase-m
              [[vars1 expr] (annotate-expr glob-state vars (second code))]
              [vars1 [:sexp expr]]
              )
      :empty (util/succ [vars code]))))

(defn check-type
  [glob-state funexpr]
  (match/match funexpr [fun get-type [ident name] args block]
    (m/domonad util/phase-m
      [
       [_ vars1] (add-var (vars-map) "_return_" get-type)
       [nargs vars] (add-args vars1 args)
       [nvars nblock] (annotate-code glob-state vars block)
       ]
      (with-vars
        [fun get-type [ident name] nargs nblock]
        nvars)
      )))

(defn analyze-fun
  [glob-state funexpr]
  (match/match funexpr [_ [get-type] [_ name] args block]
    (if (not (or (= get-type :void) (util/returns? block)))
      (util/err (str "return not found in function " name "\n" (util/ip-meta funexpr)))
      (check-type glob-state funexpr)
      )
    )
  )

(defn check
  [glob-state]
  (fn [expr]
    (m/domonad util/phase-m
      [state glob-state
       res (match/match (first expr)
             ;:clssdef (analyze-class state (second expr))
             :fndef (analyze-fun state expr))]
      res)))



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
       n-glob-state (main-check new-glob-state)
       result (reduce merge-checks (m-result []) (map (check (m-result n-glob-state)) tree))
       ]
      result
      )))
