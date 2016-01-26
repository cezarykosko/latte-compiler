(ns latte-compiler.static-analysis
  (:require
    [clojure.core.match :refer [match]]
    [clojure.algo.monads :refer [domonad]]
    [latte-compiler.util :refer [succ err ip-meta phase-m third fourth returns? toposort]]
    )
  (:use [latte-compiler.state]))

(defn- merge-checks
  [acc cur]
  (domonad phase-m
    [acc acc
     curr cur]
    (conj acc curr)
    ))

(defn- print-var
  [var]
  (match var
    [:vident [:ident name]] (str name)
    [:ident name] (str name)
    :else (str var)
    ))

(defn- vars-map
  []
  [(list (hash-map)) [0 0] 0 (hash-map)])

(defn- add-var
  [[map [num-vars num-args] num-strings strings] var type]
  (let
    [curr-scope (peek map)
     new-scope (conj curr-scope [var [type (- (+ 1 num-vars))]])
     ]
    (if (contains? curr-scope var)
      (err (str "var " (print-var var) " already declared in " (ip-meta var)))
      (succ [(- (+ 1 num-vars)) [(conj (pop map) new-scope) [(+ num-vars 1) num-args] num-strings strings]]))
    )
  )

(defn- add-string
  [[map [num-vars num-args] num-strings strings] string]
  (if (contains? strings string)
    [(second (find strings string)) [map [num-vars num-args] num-strings strings]]
    [(+ num-strings 1) [map [num-vars num-args] (+ num-strings 1) (assoc (assoc strings string (+ num-strings 1)) (+ num-strings 1) string)]]
    )
  )

(defn- add-arg-var
  [[map [num-vars num-args] num-strings strings] var type]
  (let
    [curr-scope (peek map)
     new-scope (conj curr-scope [var [type (+ 1 num-args)]])
     ]
    (if (contains? curr-scope var)
      (err (str "var " (print-var var) " already declared in " (ip-meta var)))
      (succ [(+ 1 num-vars) [(conj (pop map) new-scope) [num-vars (+ num-args 1)] num-strings strings]]))
    )
  )

(defn- lookup-var
  [[map a b c] var location]
  (if (empty? map)
    (err (str "var " (print-var var) " not found in: " location))
    (let
      [type (find (peek map) var)]
      (if (nil? type)
        (recur [(pop map) a b c] var location)
        (succ (second type))
        )
      )
    )
  )

(defn- lookup-fun
  [glob-state ident location]
  (let
    [funs (.-funs glob-state)
     rec (find funs ident)]
    (if (nil? rec)
      (err (str "function " (print-var ident) " not found in: " location))
      (succ (second rec))
      )))

(defn- lookup-field
  [clss fields fident location]
  (loop
    [fields fields]
    (if (empty? fields)
      (err (str "field " (print-var clss) "." (print-var fident) " not found in: " location))
      (let
        [map (first fields)
         res (find map fident)]
        (if (nil? res)
          (recur (rest fields))
          (succ (second res))
          )
        )))
  )

(defn- lookup-clss-field
  [glob-state clss fident location]
  (match clss
    [:atype _] (recur glob-state [:ident "_arr"] fident location)
    [:ident type]
    (let
      [classes (.-classes glob-state)
       class (find classes clss)
       ]
      (if (nil? class)
        (err (str "class " (print-var clss) " not found in: " location))
        (lookup-field clss (.-fields (second class)) fident location)
        )
      ))

  )

(defn- new-scope
  [[map num-vars num-strings strings]]
  [(conj map (hash-map)) num-vars num-strings strings])

(defn- rm-scope
  [[map num-vars num-strings strings]]
  [(pop map) num-vars num-strings strings])

(defn- makeclassdefmap
  [glob-state startmap name classdefs]
  glob-state)

(defn- makeclassdef
  [glob-state clssexpr extends]
  (match [extends (nth clssexpr 2)]
    [true [:tident [:ident ident]]]
    (let [parent-map (get (:classes glob-state) ident)]
      (if (nil? parent-map)
        (assoc glob-state :violations true)

        (makeclassdefmap glob-state parent-map (second clssexpr) (nth clssexpr 3)))
      )
    [false _] (makeclassdefmap glob-state (hash-map) (second clssexpr) (nth clssexpr 2))
    )
  )

(defn- add-ident
  [type location]
  (fn
    [fields ident]
    (domonad phase-m
      [[fields len] fields
       res (if (contains? fields ident)
             (err (str "field " (print-var ident) " already declared in: " location))
             (succ [(assoc fields ident [type len]) (+ len 1)])
             )
       ]
      res
      )))

(defn- appclassdefmap
  [glob-state [fields funs len fundefs] clssdecl]
    (match (first clssdecl)
      :fdecl
      (domonad phase-m
        [type (m-result (second clssdecl))
         idents (m-result (rest (rest clssdecl)))
         [nfields nlen] (reduce (add-ident type (ip-meta clssdecl)) (m-result [fields len]) idents)
         ]
        [nfields funs nlen fundefs])
      ))

(defn- clssdefmpstep
  [glob-state]
  (fn
    [acc val]
    (domonad phase-m
      [args  acc
       res (appclassdefmap glob-state args val)]
      res
      ))
  )

(defn- clssname
  [clssexpr]
  (second clssexpr))

(defn- clssdef
  [glob-state ident clssdecls fields funs len]
  (domonad phase-m
    [inacc (m-result [{} {} len ()])
     [nfields nfuns nlen fundefs] (reduce (clssdefmpstep glob-state) (m-result inacc) clssdecls)
     nclssdef (m-result (->ClassDef (conj fields nfields) (conj funs nfuns) nlen))
     ]
    [nclssdef fundefs]

    )
  )

(defn- makeclassdef
  [glob-state clssexpr]
  (if (nil? (find (.-classes glob-state) (second clssexpr)))
    (match (first clssexpr)
      :noextclssdef (clssdef glob-state (second clssexpr) (rest (rest clssexpr)) '() '() 0)

      )
    (err (str "class " (print-var (clssname clssexpr)) " already defined in: " (ip-meta clssexpr))))
  )

(defn- addclss-red-step
  [glob-state clssexpr]
  (domonad phase-m
    [glob-state glob-state
     [nclssdef fundefs] (makeclassdef glob-state clssexpr)
     glob-state1 (m-result (update glob-state :classes (fn [classes] (assoc classes (second clssexpr) nclssdef))))
     glob-state2 (m-result (update glob-state1 :funs (fn [funs] (reduce conj funs fundefs))))
     ]
    glob-state2))


(defn class-dep
  [clsdef]
  (match (first clsdef)
    :noextclssdef nil
    :extclssdef (third clsdef)
    ))

(defn class-name
  [clsdef]
  (second clsdef))


(defn map-type
  [type]
  (match (first type)
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
  (match fun
    [_ type name argz _]
    (let [fundef (->FunDef name (map-type type) (map map-arg (rest argz)))]
      [name (with-meta fundef (meta fun))]))
  )

(defn- conj-hlp
  [coll elem]
  (match elem
    [name fndef] (if (contains? coll name)
                   (err (str "function " (second name) " defined more than once in: " (ip-meta fndef)))
                   (succ (conj coll elem))
                   ))
  )

(defn- m-conj
  [mcoll melem]
  (domonad phase-m
    [
     coll mcoll
     elem melem
     res (conj-hlp coll elem)]
    res))

(defn- funsred
  ([funs coll]
   (domonad phase-m
     [mfuns (m-result funs)
      nfuns (succ (map map-fun funs))
      res (reduce m-conj (m-result coll) (map succ nfuns))]
     res)))

(defn- bucketize
  [buffer expr]
  (match (first expr)
    :noextclssdef
    [(conj (first buffer) expr) (second buffer)]
    :extclssdef
    [(conj (first buffer) expr) (second buffer)]
    :fndef
    [(first buffer) (conj (second buffer) expr)]
    )
  )

(defn- main-check
  [glob-state]
  (if (=
        (second (find (.-funs glob-state) [:ident "main"]))
        (->FunDef [:ident "main"] [:int] []))
    (succ glob-state)
    (err "no correct main function found")
    )
  )

(defn- add-arg
  [vmap arg]
  (match arg [:arg type ident]
    (domonad phase-m
      [[num vmap2] (add-arg-var vmap ident type)]
      [[:arg type [:ident num]] vmap2]
      )
    )
  )

(defn- add-args
  [vmap args]
  (reduce (fn [buf arg]
            (domonad phase-m
              [[tmprgs vmap2] buf
               [narg vmap3] (add-arg vmap2 arg)] [(conj tmprgs narg) vmap3]))
    [:succ [[:args] vmap]] (rest args))
  )

(defn- print-type
  [type]
  (match type
    [:void] "void"
    [:int] "int"
    [:string] "string"
    [:bool] "bool"
    [:atype ident] (str (print-type ident) "[]")
    [:tident [:ident a]] a))

(defn- print-args
  [args]
  (if (empty? args)
    "none"
    (clojure.string/join "," (map print-type args))
    )
  )

(defn- check-types
  [exp-type actual-type res location]
  (if (= actual-type exp-type)
    (succ res)
    (err (str "return type invalid; expected " (print-type exp-type) ", found " (print-type actual-type) " in " location))))

(defn- with-type
  [obj type]
  (with-meta obj (assoc (meta obj) "_type" type))
  )

(defn- get-type
  [obj]
  (second (find (meta obj) "_type")))

(defn- with-vars
  [obj vars]
  (with-meta obj (assoc (meta obj) "_vars" vars))
  )

(defn- is-str
  [x]
  (= (get-type x) [:string]))

(defn- is-bool
  [x]
  (= (get-type x) [:bool]))

(defn- is-int
  [x]
  (= (get-type x) [:int]))

(defn- is-void
  [x]
  (= (get-type x) [:void]))

(defn- annotate-erel
  [vars lexpr rexpr relop location]
  (match [lexpr rexpr relop]
    [(lexpr :guard #(is-str %)) (rexpr :guard #(is-str %)) [:eq]]
    (succ [vars (with-type [:eapp [:ident "_eqStrings"] [lexpr rexpr]] [:bool])])

    [(lexpr :guard #(is-str %)) (rexpr :guard #(is-str %)) [:ieq]]
    (succ [vars (with-type [:not [:eapp [:ident "_eqStrings"] [lexpr rexpr]]] [:bool])])

    [(lexpr :guard #(is-int %)) (rexpr :guard #(is-int %)) relop]
    (succ [vars (with-type [:erel lexpr relop rexpr] [:bool])])

    [lexpr rexpr (:or [:eq] [:ieq])]
    (if (= (get-type lexpr)
          (get-type rexpr))
      (succ [vars (with-type [:erel lexpr relop rexpr] [:bool])])
      (err (str "expr invalid; expected " (get-type lexpr) ", found " (get-type rexpr)
             " in: " location))
      )

    :else
    (err (str "expr invalid; expected int,int, "
           "found " (print-type (get-type lexpr)) ", " (print-type (get-type rexpr)) " in: " location))
    ))

(defn- annotate-eadd
  [vars lexpr rexpr relop location]
  (match [lexpr rexpr relop]
    [(lexpr :guard #(is-str %)) (rexpr :guard #(is-str %)) [:plus]]
    (succ [vars (with-type [:eapp [:ident "_concatStrings"] [lexpr rexpr]] [:string])])

    [(lexpr :guard #(is-int %)) (rexpr :guard #(is-int %)) relop]
    (succ [vars (with-type [:eadd lexpr relop rexpr] [:int])])

    [_ _ [:plus]]
    (err (str "expr invalid; expected int,int or string,string, "
           "found " (print-type (get-type lexpr)) ", " (print-type (get-type rexpr)) " in: " location))

    :else
    (err (str "expr invalid; expected int,int, "
           "found " (print-type (get-type lexpr)) ", " (print-type (get-type rexpr)) " in: " location)
      )))

(defn- annotate-eident
  [glob-state vars eident location an-expr]
  (match eident
    [:vident [:ident name]] (domonad phase-m
                              [[type num] (lookup-var vars [:ident name] location)]
                              [vars (with-type [:vident [:ident num]] type)]
                              )
    [:fident neident name] (domonad phase-m
                            [[vars2 nident] (an-expr glob-state vars neident)
                             [type offset] (lookup-clss-field glob-state (get-type nident) name location)]
                             [vars2 (with-type [:fident nident [:elitint offset]] type)]
                             )
    [:aident neident expr] (domonad phase-m
                            [[vars2 nident] (an-expr glob-state vars neident)
                             [vars3 nexpr] (an-expr glob-state vars2 expr)
                             tmp (check-types [:int] (get-type nexpr) "ok" location)
                             res (match (get-type nident)
                                   [:atype intype]
                                   (succ [vars2 (with-type [:fident nident [:eadd [:elitint 1] [:plus] nexpr]] intype)])
                                   :else
                                   (err (str "expected array type, found " (get-type nident) " in " location))
                                   )]
                            res
                            )
    [:ident name] (domonad phase-m
                    [[type num] (lookup-var vars [:ident name] location)]
                    [vars (with-type [:vident [:ident num]] type)]
                    )

    )
  )

(defn- annotate-evar
  [glob-state vars ident location an-expr]
  (domonad phase-m
    [out (annotate-eident glob-state vars ident location an-expr)
     [vars2 nident] (m-result out)
     type (m-result (get-type nident))
     res (succ [vars2 (with-type [:evar nident] type)])]
    res
    ))

(defn- annotate-expr
  [glob-state vars expr]
  (let [location (ip-meta expr)]
    (match (first expr)
      :elitint (if (or (> (second expr) 2147483647) (< (second expr) -2147483648))
                 (err (str "int literal not in the [-2147483648, 2147483647] range in " location))
                 (succ [vars (with-type expr [:int])]))
      :elittrue (succ [vars (with-type expr [:bool])])
      :elitfalse (succ [vars (with-type expr [:bool])])
      :earrlit (domonad phase-m
                 [[nvars ins-expr] (annotate-expr glob-state vars (third expr))
                  type (m-result (second expr))
                  _ (if (is-int ins-expr) (succ "")
                                          (err (str "expr type invalid; expected int, found "
                                                 (print-type (get-type ins-expr)) " in: " location)))
                  _ (match ins-expr
                      [:elitint x] (if (< x 0)
                                     (err (str "array size declared negative in: " location))
                                     (succ "ok"))
                      :else (succ "ok"))

                  ]
                 [nvars (with-type [:earrlit type ins-expr] [:atype type])])
      :etypednull (domonad phase-m
                    [type (m-result (second expr))
                     tmp (if (contains? ["void" "int" "boolean" "string"] (second type))
                           (err (str "expected class type, found " (print-var type) " in: " location))
                           (succ "ok")
                           )
                     tmp2 (if (contains? (.-classes glob-state) type)
                            (succ "ok")
                            (err (str "type " (print-var type) " not found in: " location)))
                     ]
                    [vars (with-type [:etypednull type] type)]
                    )
      :eclassinit (domonad phase-m
                    [type (m-result (second expr))
                     tmp (if (contains? ["void" "int" "boolean" "string"] (second type))
                           (err (str "expected class type, found " (print-var type) " in: " location))
                           (succ "ok")
                           )
                     tmp2 (if (contains? (.-classes glob-state) type)
                            (succ "ok")
                            (err (str "type " (print-var type) " not found in: " location)))]
                    [vars (with-type [:eclassinit type] type)]
                    )
      :estring (let
                 [[num nvar] (add-string vars (second expr))]
                 (succ [nvar (with-type [:estring num] [:string])]))
      :evar (annotate-evar glob-state vars (second expr) location annotate-expr)
      :neg (domonad phase-m
             [[nvars inside-expr] (annotate-expr glob-state vars (second expr))
              res (if (= (get-type inside-expr) [:int])
                    (succ [nvars (with-type [:neg inside-expr] [:int])])
                    (err (str "expr invalid; expected int, found " (print-type (get-type inside-expr)) " in: " location))
                    )]
             res
             )
      :not (domonad phase-m
             [[nvars inside-expr] (annotate-expr glob-state vars (second expr))
              res (if (= (get-type inside-expr) [:bool])
                    (succ [nvars (with-type [:not inside-expr] [:bool])])
                    (err (str "expr invalid; expected bool, found " (print-type (get-type inside-expr)) " in: " location))
                    )]
             res
             )
      :eor (domonad phase-m
             [[vars1 lexpr] (annotate-expr glob-state vars (second expr))
              [vars2 rexpr] (annotate-expr glob-state vars1 (third expr))
              res (if (and
                        (= (get-type lexpr) [:bool])
                        (= (get-type rexpr) [:bool]))
                    (succ [vars2 (with-type [:eor lexpr rexpr] [:bool])])
                    (err (str "expr invalid; expected bool, bool, found " (print-type (get-type lexpr)) ", " (print-type (get-type rexpr)) " in: " location))
                    )
              ]
             res)
      :eand (domonad phase-m
              [[vars1 lexpr] (annotate-expr glob-state vars (second expr))
               [vars2 rexpr] (annotate-expr glob-state vars1 (third expr))
               res (if (and
                         (= (get-type lexpr) [:bool])
                         (= (get-type rexpr) [:bool]))
                     (succ [vars2 (with-type [:eand lexpr rexpr] [:bool])])
                     (err (str "expr invalid; expected bool,bool, found " (print-type (get-type lexpr)) ", " (print-type (get-type rexpr)) " in: " location))
                     )
               ]
              res)
      :erel (domonad phase-m
              [[vars1 lexpr] (annotate-expr glob-state vars (second expr))
               [vars2 rexpr] (annotate-expr glob-state vars1 (fourth expr))
               op (m-result (third expr))
               res (annotate-erel vars2 lexpr rexpr op location)
               ]
              res)
      :emul (domonad phase-m
              [[vars1 lexpr] (annotate-expr glob-state vars (second expr))
               [vars2 rexpr] (annotate-expr glob-state vars1 (fourth expr))
               op (m-result (third expr))
               res (if (and
                         (= (get-type lexpr) [:int])
                         (= (get-type rexpr) [:int]))
                     (succ [vars2 (with-type [:emul lexpr op rexpr] [:int])])
                     (err (str "expr invalid; expected int,int, found " (print-type (get-type lexpr)) ", " (print-type (get-type rexpr)) " in: " location))
                     )
               ]
              res)
      :eadd (domonad phase-m
              [[vars1 lexpr] (annotate-expr glob-state vars (second expr))
               [vars2 rexpr] (annotate-expr glob-state vars1 (fourth expr))
               op (m-result (third expr))
               res (annotate-eadd vars2 lexpr rexpr op location)
               ]
              res)
      :eapp (domonad phase-m
              [ident (m-result (second expr))
               [nvar args] (reduce (fn [buff tmp] (domonad phase-m [[tmvar tmargs] buff [ntmvar nexpr] (annotate-expr glob-state tmvar tmp)] [ntmvar (conj tmargs nexpr)])) (m-result [vars []]) (rest (rest expr)))
               actargtypes (m-result (vec (map get-type args)))
               fundef (lookup-fun glob-state ident location)
               expargtypes (m-result (.-inTypes fundef))
               outtype (m-result (.-outType fundef))
               res (if (= expargtypes actargtypes)
                     (succ [nvar (with-type [:eapp ident args] outtype)])
                     (err (str "function invocation invalid; expected " (print-args expargtypes) " found " (print-args actargtypes) " in " location))
                     )
               ]
              res
              )
      ))
  )

(defn- process-decl
  [glob-state type]
  (fn [vars decl]
    (match decl
      [:noinit ident] (domonad phase-m
                        [
                         [tvars decls] vars
                         [num [a b c d]] (add-var tvars ident type)
                         ]
                        [[a b c d] (conj decls [:noinit [:ident num]])]
                        )
      [:init ident expr] (domonad phase-m
                           [
                            [tvars decls] vars
                            [ntvars nexpr] (annotate-expr glob-state tvars expr)
                            ntype (m-result (get-type nexpr))
                            [num [a b c d]] (add-var ntvars ident type)
                            res (if (= type ntype)
                                  (succ [[a b c d] (conj decls [:init [:ident num] nexpr])])
                                  (err (str "declaration invalid; expected " (print-type type) ", found " (print-type ntype) " in " (ip-meta decl))))]
                           res
                           )
      )))

(defn- annotate-ass
  [glob-state vars location eident expr]
  (match eident
    [:vident name]
    (domonad phase-m
      [[type num] (lookup-var vars name location)
       res (check-types type (get-type expr) [vars (with-type [:ass [:ident num] expr] (get-type expr))] location)]
      res)
    [:ident name]
    (domonad phase-m
      [[type num] (lookup-var vars [:ident name] location)
       res (check-types type (get-type expr) [vars (with-type [:ass [:ident num] expr] (get-type expr))] location)]
      res)
    [:aident neident iexpr]
    (domonad phase-m
      [[vars1 neident] (annotate-expr glob-state vars neident)
       [vars2 nexpr] (annotate-expr glob-state vars1 iexpr)
       etype (m-result (get-type expr))
       idtype (m-result (get-type neident))
       tmp (check-types [:atype etype] idtype "ok" location)
       tmp2 (check-types [:int] (get-type nexpr) "ok" location)
       ]
      [vars2 (with-type [:ass [:fident neident [:eadd [:elitint 1] [:plus] nexpr]] expr] etype)]
      )
    [:fident nident ident]
    (domonad phase-m
      [[vars1 neident] (annotate-expr glob-state vars nident)
       tmp (match (get-type neident)
             [:atype _] (err (str "array field 'length' not assignable in: " location))
             :else (succ "ok"))
       [type offset] (lookup-clss-field glob-state (get-type neident) ident location)
       tmp (check-types type (get-type expr) "ok" location)
       ]
      [vars1 (with-type [:ass [:fident neident [:elitint offset]] expr] type)]
      )
    ))

(defn- annotate-incr
  [glob-state vars location eident key]
  (match eident
    [:vident name]
    (domonad phase-m
      [[type num] (lookup-var vars name location)
       res (check-types [:int] type [vars (with-type [key [:ident num]] [:int])] location)]
      res)
    [:ident name]
    (domonad phase-m
      [[type num] (lookup-var vars [:ident name] location)
       res (check-types [:int] type [vars (with-type [key [:ident num]] [:int])] location)]
      res)
    [:aident nident iexpr]
    (domonad phase-m
      [[vars1 neident] (annotate-expr glob-state vars nident)
       [vars2 nexpr] (annotate-expr glob-state vars1 iexpr)
       etype (m-result [:int])
       idtype (m-result (get-type neident))
       tmp (check-types [:atype etype] idtype "ok" location)
       tmp2 (check-types [:int] (get-type nexpr) "ok" location)]
      [vars2 (with-type [key [:fident neident [:eadd [:elitint 1] [:plus] nexpr]]] etype)]
      )
    ))

(defn- annotate-code
  [glob-state vars code]
  (let [location (ip-meta code)]
    (match (first code)
      :block (domonad phase-m
               [[avars result] (reduce (fn [env code]
                                         (domonad phase-m
                                           [[vars blk] env
                                            [nvars res] (annotate-code glob-state vars code)]
                                           [nvars (conj blk res)]
                                           )
                                         ) (m-result [(new-scope vars) [:block]]) (rest code))]
               [(rm-scope avars) result]
               )
      :vret (domonad phase-m
              [[type _] (lookup-var vars "_return_" location)
               res (check-types type [:void] [vars [:vret]] location)]
              res)
      :ret (domonad phase-m
             [[nvars expr] (annotate-expr glob-state vars (second code))
              [type _] (lookup-var nvars "_return_" location)
              tmp (if (= type [:void]) (err (str "returning void function not allowed in " (ip-meta location))) (succ ""))
              res (check-types type (get-type expr) [nvars (with-type [:ret expr] (get-type expr))] location)
              ]
             res)
      :incr (annotate-code glob-state vars (with-meta [:ass (second code) [:eadd [:evar (second code)] [:plus] [:elitint 1]]] (meta code)))
      :decr (annotate-code glob-state vars (with-meta [:ass (second code) [:eadd [:evar (second code)] [:minus] [:elitint 1]]] (meta code)))


      #_(annotate-incr glob-state vars location (second code) :incr)
      #_:decr #_(annotate-incr glob-state vars location (second code) :decr)
      :decl (domonad phase-m
              [type (m-result (second code))
               decls (m-result (rest (rest code)))
               [v e] (reduce (process-decl glob-state type) (m-result [vars [:decl type]]) decls)]
              [v (with-type e type)]
              )
      :ass (domonad phase-m
             [[nvars expr] (annotate-expr glob-state vars (third code))
              res (annotate-ass glob-state nvars location (second code) expr)
              ]
             res)
      :cond (domonad phase-m
              [
               [vars1 expr] (annotate-expr glob-state vars (second code))
               [vars2 nblock] (annotate-code glob-state (new-scope vars1) (third code))
               res (check-types [:bool] (get-type expr) [(rm-scope vars2) [:cond expr nblock]] location)]
              res)
      :condelse (domonad phase-m
                  [
                   [vars1 expr] (annotate-expr glob-state vars (second code))
                   [vars2 nblock1] (annotate-code glob-state (new-scope vars1) (third code))
                   [vars3 nblock2] (annotate-code glob-state (new-scope (rm-scope vars2)) (fourth code))
                   res (check-types [:bool] (get-type expr) [(rm-scope vars3) [:condelse expr nblock1 nblock2]] location)]
                  res)
      :while (domonad phase-m
               [
                [vars1 expr] (annotate-expr glob-state vars (second code))
                [vars2 nblock] (annotate-code glob-state (new-scope vars1) (third code))
                res (check-types [:bool] (get-type expr) [(rm-scope vars2) [:while expr nblock]] location)]
               res
               )
      :sexp (domonad phase-m
              [[vars1 expr] (annotate-expr glob-state vars (second code))]
              [vars1 [:sexp expr]]
              )
      :for (match code
             [:for type ident eident stmt]
             (domonad phase-m
               [[_ neident] (annotate-expr glob-state vars eident)
                res
                (if (= (first (get-type neident)) :atype)
                  (annotate-code glob-state vars (with-meta
                                                   [:block
                                                    [:decl [:int] [:noinit ident]]
                                                    [:while [:erel [:evar [:vident ident]] [:lth] [:evar [:fident eident [:ident "length"]]]]
                                                     [:block
                                                      [:block
                                                       [:decl type [:init ident [:evar [:aident eident [:evar [:vident ident]]]]]]
                                                       [:block stmt]]
                                                      [:incr [:vident ident]]]]]
                                                   (meta code)))
                  (err (str "for statement on non-array type in: " location))
                  )]
               res))
      :empty (succ [vars code]))))

(defn- check-type
  [glob-state funexpr]
  (match funexpr [fun get-type [ident name] args block]
    (domonad phase-m
      [
       [_ vars1] (add-arg (vars-map) [:arg get-type "_return_"])
       [nargs vars] (add-args vars1 args)
       [nvars nblock] (annotate-code glob-state vars block)
       ]
      (with-vars
        [fun get-type [ident name] nargs nblock]
        nvars)
      )))

(defn- analyze-fun
  [glob-state funexpr]
  (match funexpr [_ get-type [_ name] _ block]
    (if (not (or (= get-type [:void]) (returns? block)))
      (err (str "return not found in function " name "\n" (ip-meta funexpr)))
      (check-type glob-state funexpr)
      )
    )
  )

(defn- check
  [glob-state]
  (fn [expr]
    (domonad phase-m
      [state glob-state
       res (match (first expr)
             ;:clssdef (analyze-class state (second expr))
             :fndef (analyze-fun state expr))
       ]
      res)))

(defn analize
  [tree]
  (let
    [glob-state default-state
     [split-clss split-funs] (reduce bucketize [[] []] tree)
     classes (toposort (vec split-clss) class-name class-dep)
     ]
    (domonad phase-m
      [
       funs (funsred split-funs (.-funs glob-state))
       new-glob-state (m-result (update glob-state :funs (fn [_] funs)))
       fin-glob-state (reduce addclss-red-step (m-result new-glob-state) classes)
       n-glob-state (main-check fin-glob-state)
       result (reduce merge-checks (m-result []) (map (check (m-result n-glob-state)) split-funs))
       ]
      [n-glob-state result]
      )))
