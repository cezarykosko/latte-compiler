(ns latte-compiler.static-analysis
  (:require
    [clojure.core.match :refer [match]]
    [clojure.algo.monads :refer [domonad]]
    [latte-compiler.util :refer [succ err ip-meta phase-m third fourth returns? toposort]]
    )
  (:use [latte-compiler.state]))

(defn- print-var
  [var]
  (match var
    [:vident [:ident name]] (str name)
    [:ident name] (str name)
    :else (str var)))

(defn- print-type
  [type]
  (match type
    [:void] "void"
    [:int] "int"
    [:string] "string"
    [:bool] "bool"
    [:atype ident] (str (print-type ident) "[]")
    [:tident [:ident a]] a
    [:ident a] a))

(defn map-fun
  [fun]
  (match fun
    [_ type name argz _]
    (let [fundef (->FunDef name type (map second (rest argz)))]
      [name (with-meta fundef (meta fun))])))

(defn- equiv-void?
  [type]
  (match type
    [:atype x] (equiv-void? x)
    [:ident "void"] true
    :else false))

(defn map-clss-fun
  [fun clzname]
  (match fun
    [_ type name argz _]
    (let [fundef (->FunDef (str "_" (print-var clzname) "_" (print-var name)) type (map second (rest argz)))]
      (with-meta fundef (meta fun)))))

(defn- check-types-helper
  [glob-state exp-type actual-type]
  (match [exp-type actual-type]
    [[:atype a] [:atype b]] (check-types-helper glob-state a b)
    [[:ident t1] [:ident t2]] (or (= t1 t2)
                                (let
                                  [classes (.-cls-parents glob-state)
                                   parents (second (find classes [:ident t2]))]
                                  (not (nil? (reduce #(when (= %2 [:ident t1]) (reduced %2)) nil parents)))))
    [a b] (= a b)))

(defn- list-match-types?
  [glob-state l-exp l-act]
  (and (= (count l-exp) (count l-act))
    (reduce
      #(and %1 (check-types-helper glob-state (first %2) (second %2)))
      true (map vector l-exp l-act))))

(defn- merge-checks
  [acc cur]
  (domonad phase-m
    [acc acc
     curr cur]
    (conj acc curr)))

(defn- vars-map []
  [(list (hash-map)) [0 0] 0 (hash-map)])

(defn- add-var
  [[map [num-vars num-args] num-strings strings] var type]
  (let
    [curr-scope (peek map)
     new-scope (conj curr-scope [var [type (- (+ 1 num-vars))]])]
    (if (contains? curr-scope var)
      (err (str "var " (print-var var) " already declared in " (ip-meta var)))
      (succ [(- (+ 1 num-vars)) [(conj (pop map) new-scope) [(+ num-vars 1) num-args] num-strings strings]]))))

(defn- add-string
  [[map [num-vars num-args] num-strings strings] string]
  (if (contains? strings string)
    [(second (find strings string)) [map [num-vars num-args] num-strings strings]]
    [(+ num-strings 1) [map [num-vars num-args] (+ num-strings 1) (assoc (assoc strings string (+ num-strings 1)) (+ num-strings 1) string)]]))

(defn- add-arg-var
  [[map [num-vars num-args] num-strings strings] var type]
  (let
    [curr-scope (peek map)
     new-scope (conj curr-scope [var [type (+ 1 num-args)]])]
    (if (contains? curr-scope var)
      (err (str "var " (print-var var) " already declared in " (ip-meta var)))
      (succ [(+ 1 num-vars) [(conj (pop map) new-scope) [num-vars (+ num-args 1)] num-strings strings]]))))

(defn- safe-lookup-var
  [[map a b c] var]
  (if (empty? map)
    nil
    (let
      [type (find (peek map) var)]
      (if (nil? type)
        (recur [(pop map) a b c] var)
        (second type)))))

(defn- lookup-var
  [[map a b c] var location]
  (let
    [res (safe-lookup-var [map a b c] var)]
    (if (nil? res)
      (err (str "var " (print-var var) " not found in: " location))
      (succ res))))

(defn- lookup-cls-fun
  [glob-state type ident location reiterated?]
  (let
    [classes (.-classes glob-state)
     rec (find classes type)
     errmsg
     (err (str "function" (if reiterated? (str "s " (print-var ident) " and ") " ") (print-var type) "." ident " not found in: " location))]
    (if (nil? rec)
      errmsg
      (let
        [class (second rec)
         funs (.-funs class)
         fun (find funs ident)]
        (if (nil? fun)
          errmsg
          (succ (second fun)))))))

(defn- lookup-fun
  [glob-state ident]
  (let
    [funs (.-funs glob-state)
     rec (find funs ident)]
    (if (nil? rec)
      nil
      (second rec))))

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
          (succ (second res)))))))

(defn- lookup-clss-field
  [glob-state clss fident location]
  (match clss
    [:atype _] (recur glob-state [:ident "_arr"] fident location)
    [:ident type]
    (let
      [classes (.-classes glob-state)
       class (find classes clss)]
      (if (nil? class)
        (err (str "class " (print-var clss) " not found in: " location))
        (lookup-field clss (.-fields (second class)) fident location)))
    type
    (err (str "expected class type, found " (print-type type) " in: " location))))

(defn- new-scope
  [[map num-vars num-strings strings]]
  [(conj map (hash-map)) num-vars num-strings strings])

(defn- rm-scope
  [[map num-vars num-strings strings]]
  [(pop map) num-vars num-strings strings])

(defn- add-ident
  [type location]
  (fn
    [fields ident]
    (domonad phase-m
      [[fields len] fields
       tmp (if (equiv-void? type)
             (err (str "void field declared in: " location))
             (succ "ok"))
       tmp2 (if (= ident [:ident "self"])
              (err (str "class field may not be named 'self', in: " location))
              (succ "ok"))
       res (if (contains? fields ident)
             (err (str "field " (print-var ident) " already declared in: " location))
             (succ [(assoc fields ident [type len]) (+ len 1)]))]
      res)))

(defn- fundefs-equivalent?
  [glob-state expected found]
  (and
    (check-types-helper glob-state (.-outType expected) (.-outType found))
    (list-match-types? glob-state (.-inTypes found) (.-inTypes expected))))

(defn- appclassdefmap
  [glob-state [fields funs len fundefs] clssdecl clssname]
  (match (first clssdecl)
    :fdecl
    (domonad phase-m
      [type (m-result (second clssdecl))
       idents (m-result (rest (rest clssdecl)))
       [nfields nlen] (reduce (add-ident type (ip-meta clssdecl)) (m-result [fields len]) idents)]
      [nfields funs nlen fundefs])

    :fndef
    (match clssdecl
      [_ type ident args block]
      (let [fundef (map-clss-fun clssdecl clssname)
            rec (find funs ident)
            newfndef
            [:fndef type [:ident (str "_" (print-var clssname) "_" (print-var ident))] (vec (concat [:args] (vec (conj (into '() (rest args)) [:arg clssname [:ident "self"]])))) block]
            ]
        (if (nil? rec)
          (succ [fields (assoc funs ident [fundef len]) (+ 1 len) (conj fundefs newfndef)])
          (match (second rec)
            [supfundef offset]
            (if (fundefs-equivalent? glob-state supfundef fundef)
              (succ [fields (assoc funs ident [fundef offset]) len (conj fundefs newfndef)])
              (err (str "invalid overload in: " (ip-meta clssdecl))))))))))

(defn- clssdefmpstep
  [glob-state clssname]
  (fn
    [acc val]
    (domonad phase-m
      [args acc
       res (appclassdefmap glob-state args val clssname)]
      res)))

(defn- clssname
  [clssexpr]
  (second clssexpr))

(defn- clssdef
  [glob-state clssdecls fields funs len parents]
  (domonad phase-m
    [inacc (m-result [{} funs len ()])
     [nfields nfuns nlen fundefs] (reduce (clssdefmpstep glob-state (peek parents)) (m-result inacc) clssdecls)
     nclssdef (m-result (->ClassDef (conj fields nfields) nfuns nlen parents))]
    [nclssdef fundefs]))

(defn- makeclassdef
  [glob-state clssexpr]
  (if (nil? (find (.-classes glob-state) (second clssexpr)))
    (match (first clssexpr)
      :noextclssdef (clssdef glob-state (rest (rest clssexpr)) '() {} 1 (conj '() (second clssexpr)))
      :extclssdef (let
                    [name (second clssexpr)
                     exts (third clssexpr)
                     entry (find (.-classes glob-state) exts)
                     def (second entry)]
                    (clssdef glob-state (rest (rest (rest clssexpr))) (.-fields def) (.-funs def) (.-size def)
                      (conj (.-parents def) name))))
    (err (str "class " (print-var (clssname clssexpr)) " already defined in: " (ip-meta clssexpr)))))

(defn- addclss-red-step
  [glob-state clssexpr]
  (domonad phase-m
    [[glob-state fundefs-so-far] glob-state
     [nclssdef fundefs] (makeclassdef glob-state clssexpr)
     glob-state1 (m-result (update glob-state :classes (fn [classes] (assoc classes (second clssexpr) nclssdef))))]
    [glob-state1 (concat fundefs-so-far fundefs)]))

(defn- addprns-red-step
  [glob-state clssexpr]
  (domonad phase-m
    [glob-state glob-state
     parents (m-result (.-cls-parents glob-state))
     base-list (match (first clssexpr)
                 :noextclssdef (m-result '())
                 :extclssdef (m-result (second (find parents (third clssexpr)))))
     res (if (contains? parents (second clssexpr))
           (err (str "class " (print-var (second clssexpr)) " already defined, in: " (ip-meta clssexpr)))
           (succ (update glob-state :cls-parents (fn [parents] (assoc parents (second clssexpr) (conj base-list (second clssexpr)))))))]
    res))

(defn class-dep
  [clsdef]
  (match (first clsdef)
    :noextclssdef nil
    :extclssdef (third clsdef)))

(defn class-name
  [clsdef]
  (second clsdef))

(defn- conj-hlp
  [coll elem]
  (match elem
    [name fndef] (if (contains? coll name)
                   (err (str "function " (second name) " defined more than once in: " (ip-meta fndef)))
                   (succ (conj coll elem)))))

(defn- m-conj
  [mcoll melem]
  (domonad phase-m
    [coll mcoll
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
    [(first buffer) (conj (second buffer) expr)]))

(defn- main-check
  [glob-state]
  (if (=
        (second (find (.-funs glob-state) [:ident "main"]))
        (->FunDef [:ident "main"] [:int] []))
    (succ glob-state)
    (err "no correct main function found")))

(defn- add-arg
  [vmap arg]
  (match arg [:arg type ident]
    (domonad phase-m
      [[num vmap2] (add-arg-var vmap ident type)
       tmp (if (equiv-void? type)
             (err (str "void arg declared in: " (ip-meta arg)))
             (succ "ok"))]
      [[:arg type [:ident num]] vmap2])))

(defn- add-args
  [vmap args]
  (reduce (fn [buf arg]
            (domonad phase-m
              [[tmprgs vmap2] buf
               [narg vmap3] (add-arg vmap2 arg)] [(conj tmprgs narg) vmap3]))
    [:succ [[:args] vmap]] (rest args)))

(defn- print-args
  [args]
  (if (empty? args)
    "none"
    (clojure.string/join "," (map print-type args))))

(defn- fail-check-types
  [exp act loc]
  (err (str "type mismatch; expected " (print-type exp) ", found " (print-type act) " in " loc)))

(defn- check-types
  [glob-state exp-type actual-type res location]
  (if (check-types-helper glob-state exp-type actual-type)
    (succ res)
    (fail-check-types exp-type actual-type location)))

(defn- with-type
  [obj type]
  (with-meta obj (assoc (meta obj) "_type" type)))

(defn- with-vars
  [obj vars]
  (with-meta obj (assoc (meta obj) "_vars" vars)))

(defn- get-type
  [obj]
  (second (find (meta obj) "_type")))

(defn- is-str
  [x]
  (= (get-type x) [:string]))

(defn- is-int
  [x]
  (= (get-type x) [:int]))

(defn- annotate-erel
  [glob-state vars lexpr rexpr relop location]
  (match [lexpr rexpr relop]
    [(lexpr :guard #(is-str %)) (rexpr :guard #(is-str %)) [:eq]]
    (succ [vars (with-type [:eapp [:ident "_eqStrings"] [lexpr rexpr]] [:bool])])

    [(lexpr :guard #(is-str %)) (rexpr :guard #(is-str %)) [:ieq]]
    (succ [vars (with-type [:not [:eapp [:ident "_eqStrings"] [lexpr rexpr]]] [:bool])])

    [(lexpr :guard #(is-int %)) (rexpr :guard #(is-int %)) relop]
    (succ [vars (with-type [:erel lexpr relop rexpr] [:bool])])

    [lexpr rexpr (:or [:eq] [:ieq])]
    (if (check-types-helper glob-state (get-type lexpr)
          (get-type rexpr))
      (succ [vars (with-type [:erel lexpr relop rexpr] [:bool])])
      (err (str "expr invalid; expected " (print-type (get-type lexpr)) ", found "
             (print-type (get-type rexpr)) " in: " location)))

    :else
    (err (str "expr invalid; expected int,int, "
           "found " (print-type (get-type lexpr)) ", " (print-type (get-type rexpr)) " in: " location))))

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
           "found " (print-type (get-type lexpr)) ", " (print-type (get-type rexpr)) " in: " location))))

(defn- annotate-eident
  [glob-state vars eident location an-expr]
  (match eident
    [:vident [:ident name]] (annotate-eident glob-state vars (second eident) location an-expr)
    [:fident neident name] (domonad phase-m
                             [[vars2 nident] (an-expr glob-state vars neident)
                              [type offset] (lookup-clss-field glob-state (get-type nident) name location)]
                             [vars2 (with-type [:fident nident [:elitint offset]] type)])
    [:aident neident expr] (domonad phase-m
                             [[vars2 nident] (an-expr glob-state vars neident)
                              [vars3 nexpr] (an-expr glob-state vars2 expr)
                              tmp (check-types glob-state [:int] (get-type nexpr) "ok" location)
                              res (match (get-type nident)
                                    [:atype intype]
                                    (succ [vars2 (with-type [:fident nident [:eadd [:elitint 1] [:plus] nexpr]] intype)])
                                    :else
                                    (err (str "expected array type, found " (get-type nident) " in " location)))]
                             res)
    [:ident name] (let
                    [res (safe-lookup-var vars [:ident name])]
                    (if (nil? res)
                      (if (nil? (find (meta eident) "reiterated?"))
                        (annotate-eident glob-state vars [:fident [:evar [:vident (with-meta [:ident "self"] {"reiterated?" "yup"})]] [:ident name]] location an-expr)
                        (err (str "var not found in: " location)))
                      (domonad phase-m
                        [[type num] (m-result res)]
                        [vars (with-type [:vident [:ident num]] type)])))))

(defn- annotate-evar
  [glob-state vars ident location an-expr]
  (domonad phase-m
    [out (annotate-eident glob-state vars ident location an-expr)
     [vars2 nident] (m-result out)
     type (m-result (get-type nident))
     res (succ [vars2 (with-type [:evar nident] type)])]
    res))

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
                      :else (succ "ok"))]
                 [nvars (with-type [:earrlit type ins-expr] [:atype type])])
      :etypednull (domonad phase-m
                    [type (m-result (second expr))
                     tmp (if (contains? ["void" "int" "boolean" "string"] (second type))
                           (err (str "expected class type, found " (print-type type) " in: " location))
                           (succ "ok")
                           )
                     tmp2 (if (contains? (.-classes glob-state) type)
                            (succ "ok")
                            (err (str "type " (print-var type) " not found in: " location)))]
                    [vars (with-type [:etypednull type] type)])
      :eclassinit (domonad phase-m
                    [type (m-result (second expr))
                     tmp (if (contains? ["void" "int" "boolean" "string"] (second type))
                           (err (str "expected class type, found " (print-var type) " in: " location))
                           (succ "ok"))
                     tmp2 (if (contains? (.-classes glob-state) type)
                            (succ "ok")
                            (err (str "type " (print-var type) " not found in: " location)))]
                    [vars (with-type [:eclassinit type] type)])
      :estring (let
                 [[num nvar] (add-string vars (second expr))]
                 (succ [nvar (with-type [:estring num] [:string])]))
      :evar (annotate-evar glob-state vars (second expr) location annotate-expr)
      :neg (domonad phase-m
             [[nvars inside-expr] (annotate-expr glob-state vars (second expr))
              res (if (= (get-type inside-expr) [:int])
                    (succ [nvars (with-type [:neg inside-expr] [:int])])
                    (err (str "expr invalid; expected int, found " (print-type (get-type inside-expr)) " in: " location)))]
             res)
      :not (domonad phase-m
             [[nvars inside-expr] (annotate-expr glob-state vars (second expr))
              res (if (= (get-type inside-expr) [:bool])
                    (succ [nvars (with-type [:not inside-expr] [:bool])])
                    (err (str "expr invalid; expected bool, found " (print-type (get-type inside-expr)) " in: " location)))]
             res)
      :eor (domonad phase-m
             [[vars1 lexpr] (annotate-expr glob-state vars (second expr))
              [vars2 rexpr] (annotate-expr glob-state vars1 (third expr))
              res (if (and
                        (= (get-type lexpr) [:bool])
                        (= (get-type rexpr) [:bool]))
                    (succ [vars2 (with-type [:eor lexpr rexpr] [:bool])])
                    (err (str "expr invalid; expected bool, bool, found " (print-type (get-type lexpr)) ", " (print-type (get-type rexpr)) " in: " location)))]
             res)
      :eand (domonad phase-m
              [[vars1 lexpr] (annotate-expr glob-state vars (second expr))
               [vars2 rexpr] (annotate-expr glob-state vars1 (third expr))
               res (if (and
                         (= (get-type lexpr) [:bool])
                         (= (get-type rexpr) [:bool]))
                     (succ [vars2 (with-type [:eand lexpr rexpr] [:bool])])
                     (err (str "expr invalid; expected bool,bool, found " (print-type (get-type lexpr)) ", " (print-type (get-type rexpr)) " in: " location)))]
              res)
      :erel (domonad phase-m
              [[vars1 lexpr] (annotate-expr glob-state vars (second expr))
               [vars2 rexpr] (annotate-expr glob-state vars1 (fourth expr))
               op (m-result (third expr))
               res (annotate-erel glob-state vars2 lexpr rexpr op location)]
              res)
      :emul (domonad phase-m
              [[vars1 lexpr] (annotate-expr glob-state vars (second expr))
               [vars2 rexpr] (annotate-expr glob-state vars1 (fourth expr))
               op (m-result (third expr))
               res (if (and
                         (= (get-type lexpr) [:int])
                         (= (get-type rexpr) [:int]))
                     (succ [vars2 (with-type [:emul lexpr op rexpr] [:int])])
                     (err (str "expr invalid; expected int,int, found " (print-type (get-type lexpr)) ", " (print-type (get-type rexpr)) " in: " location)))]
              res)
      :eadd (domonad phase-m
              [[vars1 lexpr] (annotate-expr glob-state vars (second expr))
               [vars2 rexpr] (annotate-expr glob-state vars1 (fourth expr))
               op (m-result (third expr))
               res (annotate-eadd vars2 lexpr rexpr op location)]
              res)
      :fapp (domonad phase-m
              [[nvar0 eident] (annotate-expr glob-state vars (second expr))
               ident (m-result (third expr))
               [nvar args] (reduce (fn [buff tmp] (domonad phase-m [[tmvar tmargs] buff [ntmvar nexpr] (annotate-expr glob-state tmvar tmp)] [ntmvar (conj tmargs nexpr)])) (m-result [nvar0 []]) (vec (rest (rest (rest expr)))))
               [fundef offset] (lookup-cls-fun glob-state (get-type eident) ident location (find (meta expr) "from_eapp"))
               actargtypes (m-result (vec (map get-type args)))
               expargtypes (m-result (.-inTypes fundef))
               outtype (m-result (.-outType fundef))
               res (if (list-match-types? glob-state expargtypes actargtypes)
                     (succ [nvar (with-type [:eclsapp offset (conj (into '() (vec args)) eident)] outtype)])
                     (err (str "function invocation invalid; expected " (print-args expargtypes) " found " (print-args actargtypes) " in " location)))]
              res)
      :eapp (let
              [ident (second expr)
               args (rest (rest expr))
               fundef (lookup-fun glob-state ident)]
              (if (nil? fundef)
                (annotate-expr glob-state vars (with-meta (vec (concat [:fapp [:evar [:vident [:ident "self"]]]
                                                                        ident] args))
                                                 (assoc (meta expr) "reiterated?" true)))
                (domonad phase-m
                  [ident (m-result (second expr))
                   [nvar args] (reduce (fn [buff tmp] (domonad phase-m [[tmvar tmargs] buff [ntmvar nexpr] (annotate-expr glob-state tmvar tmp)] [ntmvar (conj tmargs nexpr)])) (m-result [vars []]) (rest (rest expr)))
                   actargtypes (m-result (vec (map get-type args)))
                   expargtypes (m-result (.-inTypes fundef))
                   outtype (m-result (.-outType fundef))
                   res (if (list-match-types? glob-state expargtypes actargtypes)
                         (succ [nvar (with-type [:eapp ident args] outtype)])
                         (err (str "function invocation invalid; expected " (print-args expargtypes) " found " (print-args actargtypes) " in " location)))]
                  res))))))

(defn- process-decl
  [glob-state type]
  (fn [vars decl]
    (match decl
      [:noinit ident] (domonad phase-m
                        [[tvars decls] vars
                         [num [a b c d]] (add-var tvars ident type)]
                        [[a b c d] (conj decls [:noinit [:ident num]])])
      [:init ident expr] (domonad phase-m
                           [[tvars decls] vars
                            [ntvars nexpr] (annotate-expr glob-state tvars expr)
                            ntype (m-result (get-type nexpr))
                            [num varz] (add-var ntvars ident type)
                            res (check-types glob-state type ntype
                                  [varz (conj decls [:init [:ident num] nexpr])]
                                  (ip-meta decl))]
                           res))))

(defn- annotate-ass
  [glob-state vars location eident expr]
  (match eident
    [:vident name]
    (annotate-ass glob-state vars location name expr)
    [:ident name]
    (let
      [res (safe-lookup-var vars [:ident name])]
      (if (nil? res)
        (if (nil? (find (meta eident) "reiterated?"))
          (annotate-ass glob-state vars location [:fident [:evar (with-meta [:ident "self"] {"reiterated?" "yup"})] [:ident name]] expr)
          (err (str "var not found in: " location)))
        (domonad phase-m
          [[type num] (lookup-var vars [:ident name] location)
           res (check-types glob-state type (get-type expr) [vars (with-type [:ass [:ident num] expr] (get-type expr))] location)]
          res)))
    [:aident neident iexpr]
    (domonad phase-m
      [[vars1 neident] (annotate-expr glob-state vars neident)
       [vars2 nexpr] (annotate-expr glob-state vars1 iexpr)
       etype (m-result (get-type expr))
       idtype (m-result (get-type neident))
       tmp (check-types glob-state [:atype etype] idtype "ok" location)
       tmp2 (check-types glob-state [:int] (get-type nexpr) "ok" location)]
      [vars2 (with-type [:ass [:fident neident [:eadd [:elitint 1] [:plus] nexpr]] expr] etype)])
    [:fident nident ident]
    (domonad phase-m
      [[vars1 neident] (annotate-expr glob-state vars nident)
       tmp (match (get-type neident)
             [:atype _] (err (str "array field 'length' not assignable in: " location))
             :else (succ "ok"))
       [type offset] (lookup-clss-field glob-state (get-type neident) ident location)
       tmp (check-types glob-state type (get-type expr) "ok" location)]
      [vars1 (with-type [:ass [:fident neident [:elitint offset]] expr] type)])))

(defn- annotate-code
  [glob-state vars code]
  (let [location (ip-meta code)]
    (match (first code)
      :block (domonad phase-m
               [[avars result] (reduce (fn [env code]
                                         (domonad phase-m
                                           [[vars blk] env
                                            [nvars res] (annotate-code glob-state vars code)]
                                           [nvars (conj blk res)]))
                                 (m-result [(new-scope vars) [:block]]) (rest code))]
               [(rm-scope avars) result])
      :vret (domonad phase-m
              [[type _] (lookup-var vars "_return_" location)
               res (check-types glob-state type [:void] [vars [:vret]] location)]
              res)
      :ret (domonad phase-m
             [[nvars expr] (annotate-expr glob-state vars (second code))
              [type _] (lookup-var nvars "_return_" location)
              tmp (if (= type [:void]) (err (str "returning void function not allowed in " (ip-meta location))) (succ ""))
              res (check-types glob-state type (get-type expr) [nvars (with-type [:ret expr] (get-type expr))] location)]
             res)
      :incr (annotate-code glob-state vars (with-meta [:ass (second code) [:eadd [:evar (second code)] [:plus] [:elitint 1]]] (meta code)))
      :decr (annotate-code glob-state vars (with-meta [:ass (second code) [:eadd [:evar (second code)] [:minus] [:elitint 1]]] (meta code)))
      :decl (domonad phase-m
              [type (m-result (second code))
               tmp (if (equiv-void? type)
                     (err (str "void var declared in: " location))
                     (succ "ok"))
               decls (m-result (rest (rest code)))
               [v e] (reduce (process-decl glob-state type) (m-result [vars [:decl type]]) decls)]
              [v (with-type e type)])
      :ass (domonad phase-m
             [[nvars expr] (annotate-expr glob-state vars (third code))
              res (annotate-ass glob-state nvars location (second code) expr)]
             res)
      :cond (domonad phase-m
              [[vars1 expr] (annotate-expr glob-state vars (second code))
               [vars2 nblock] (annotate-code glob-state (new-scope vars1) (third code))
               res (check-types glob-state [:bool] (get-type expr) [(rm-scope vars2) [:cond expr nblock]] location)]
              res)
      :condelse (domonad phase-m
                  [[vars1 expr] (annotate-expr glob-state vars (second code))
                   [vars2 nblock1] (annotate-code glob-state (new-scope vars1) (third code))
                   [vars3 nblock2] (annotate-code glob-state (new-scope (rm-scope vars2)) (fourth code))
                   res (check-types glob-state [:bool] (get-type expr) [(rm-scope vars3) [:condelse expr nblock1 nblock2]] location)]
                  res)
      :while (domonad phase-m
               [[vars1 expr] (annotate-expr glob-state vars (second code))
                [vars2 nblock] (annotate-code glob-state (new-scope vars1) (third code))
                res (check-types glob-state [:bool] (get-type expr) [(rm-scope vars2) [:while expr nblock]] location)]
               res)
      :sexp (domonad phase-m
              [[vars1 expr] (annotate-expr glob-state vars (second code))]
              [vars1 [:sexp expr]])
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
                  (err (str "for statement on non-array type in: " location)))]
               res))
      :empty (succ [vars code]))))

(defn- check-type
  [glob-state funexpr]
  (match funexpr [fun get-type [ident name] args block]
    (domonad phase-m
      [tmp (if (equiv-void? get-type)
             (err (str "void array type declared in " (ip-meta funexpr)))
             (succ "ok"))
       [_ vars1] (add-arg (vars-map) [:arg get-type "_return_"])
       [nargs vars] (add-args vars1 args)
       [nvars nblock] (annotate-code glob-state vars block)]
      (with-vars
        [fun get-type [ident name] nargs nblock]
        nvars))))

(defn- analyze-fun
  [glob-state funexpr]
  (match funexpr [_ get-type [_ name] _ block]
    (if (not (or (= get-type [:void]) (returns? block)))
      (err (str "return not found in function " name "\n" (ip-meta funexpr)))
      (check-type glob-state funexpr))))

(defn- check
  [glob-state]
  (fn [expr]
    (domonad phase-m
      [state glob-state
       res (analyze-fun state expr)]
      res)))

(defn analize
  [tree]
  (let
    [glob-state default-state
     [split-clss split-funs] (reduce bucketize [[] []] tree)]
    (domonad phase-m
      [classes (toposort (vec split-clss) class-name class-dep)
       funs (funsred split-funs (.-funs glob-state))
       new-glob-state (m-result (update glob-state :funs (fn [_] funs)))
       parents-glob-state (reduce addprns-red-step (m-result new-glob-state) classes)
       [fin-glob-state nsplit-funs] (reduce addclss-red-step (m-result [parents-glob-state split-funs]) classes)
       n-glob-state (main-check fin-glob-state)
       result (reduce merge-checks (m-result []) (map (check (m-result n-glob-state)) nsplit-funs))]
      [n-glob-state result])))
