(ns latte-compiler.compilation
  (:require [latte-compiler.util :refer [third fourth]]
            [clojure.core.match :refer [match]]))

(defn- ptr
  [reg]
  (str "*" reg))

(defn- const
  [name]
  (str "$" name))

(def empty-string "emptystring")

(defn- get-type
  [obj]
  (second (find (meta obj) "_type")))

(defn- default-for-type
  [type]
  (match type
    [:int] (const 0)
    [:string] (const empty-string)
    [:bool] (const 0)
    [:atype _] (const 0)
    [:ident _] (const 0)))

(defn- label-name
  [name lcount]
  (str "." name "label" lcount))

(defn- label_
  [label]
  (println (str label ":")))

(defn- pop_
  [dest]
  (println (str "\t" "popl" " " dest)))

(defn- push_
  [src]
  (println (str "\t" "pushl" " " src)))

(defn- move_
  [src dest]
  (println (str "\t" "movl" " " src ", " dest)))

(defn- call_
  [fun]
  (println (str "\t" "call" " " fun)))

(defn- add_
  [arg1 arg2]
  (println (str "\t" "addl" " " arg1 ", " arg2)))

(defn- sub_
  [arg1 arg2]
  (println (str "\t" "subl" " " arg1 ", " arg2)))

(defn- imul_
  [arg1 arg2]
  (println (str "\t" "imull" " " arg1 ", " arg2)))

(defn- idiv_
  [arg]
  (println (str "\t" "idivl" " " arg)))

(defn- cdq_ []
  (println (str "\t" "cdq")))

(defn- xor_
  [arg1 arg2]
  (println (str "\t" "xorl" " " arg1 ", " arg2)))

(defn- test_
  [arg1 arg2]
  (println (str "\t" "test" " " arg1 ", " arg2)))

(defn- cmp_
  [arg1 arg2]
  (println (str "\t" "cmp" " " arg1 ", " arg2)))

(defn- jmp_
  [label]
  (println (str "\t" "jmp" " " label)))

(defn- je_
  [label]
  (println (str "\t" "je" " " label)))

(defn- jne_
  [label]
  (println (str "\t" "jne" " " label)))

(defn- jg_
  [label]
  (println (str "\t" "jg" " " label)))

(defn- jge_
  [label]
  (println (str "\t" "jge" " " label)))

(defn- jl_
  [label]
  (println (str "\t" "jl" " " label)))

(defn- jle_
  [label]
  (println (str "\t" "jle" " " label)))

(defn- lea_
  [src dest]
  (println (str "\t" "leal" " " src ", " dest)))

(defn- leave_ []
  (println (str "\t" "leave")))

(defn- ret_ []
  (println (str "\t" "ret")))

(defn- string-addr
  [name n]
  (str name "string" n))

(defn- offset-addr
  [offset addr]
  (if (= offset 0)
    (str "(" addr ")")
    (str (* 4 offset) "(" addr ")")))

(def eax "%eax")
(def ecx "%ecx")
(def edx "%edx")
(def ebp "%ebp")
(def esp "%esp")

(defn- empty-string_ []
  (println (str empty-string ":"))
  (println (str "\t" ".string \"" "\""))
  (println (str "\t" ".text"))
  (println))

(defn- strings_
  [name strings nstrings]
  (doseq [n (range 1 (+ nstrings 1))]
    (println (str (string-addr name n) ":"))
    (println (str "\t" ".string \"" (second (find strings n)) "\""))
    (println (str "\t" ".text"))
    (println)))

(declare expr_)

(defn- get-varnum
  [glob-state name var label-count]
  (match var
    [:ident num]
    [label-count (offset-addr num ebp)]
    [:vident [:ident num]] (recur glob-state name [:ident num] label-count)

    [:fident eident expr] (let
                            [nlc (expr_ glob-state name eident label-count)
                             nlc1 (expr_ glob-state name expr nlc)]
                            (pop_ edx)
                            (imul_ (const 4) edx)
                            (pop_ eax)
                            (add_ edx eax)
                            [nlc1 (offset-addr 0 eax)])))

(defn- evar_
  [glob-state name expr label-count]
  (let
    [[nlc addr] (get-varnum glob-state name expr label-count)]
    (push_ addr)
    nlc))

(defmulti expr_
  (fn [_ _ expr _]
    (first expr)))

(defmethod expr_ :elittrue
  [_ _ _ label-count]
  (push_ (const 1))
  label-count)

(defmethod expr_ :elitfalse
  [_ _ _ label-count]
  (push_ (const 0))
  label-count)

(defmethod expr_ :elitint
  [_ _ expr label-count]
  (push_ (const (second expr)))
  label-count)

(defmethod expr_ :etypednull
  [_ _ _ label-count]
  (push_ (const 0))
  label-count)

(defmethod expr_ :estring
  [_ name expr label-count]
  (push_ (const (string-addr name (second expr))))
  label-count)

(defmethod expr_ :evar
  [glob-state name expr label-count]
  (evar_ glob-state name (second expr) label-count))

(defmethod expr_ :neg
  [glob-state name expr label-count]
  (let
    [nlc (expr_ glob-state name (second expr) label-count)]
    (pop_ edx)
    (move_ (const 0) eax)
    (sub_ edx eax)
    (push_ eax)
    nlc))

(defmethod expr_ :not
  [glob-state name expr label-count]
  (let
    [nlc (expr_ glob-state name (second expr) label-count)]
    (pop_ eax)
    (xor_ (const 1) eax)
    (push_ eax)
    nlc))

(defmethod expr_ :eor
  [glob-state name expr label-count]
  (let
    [ltrue (label-name name label-count)
     lend (label-name name (+ label-count 1))
     nlc (+ label-count 2)
     nlc1 (expr_ glob-state name (second expr) nlc)]
    (pop_ eax)
    (test_ eax eax)
    (jne_ ltrue)
    (let
      [nlc2 (expr_ glob-state name (third expr) nlc1)]
      (pop_ eax)
      (test_ eax eax)
      (jne_ ltrue)
      (push_ (const 0))
      (jmp_ lend)
      (label_ ltrue)
      (push_ (const 1))
      (label_ lend)
      nlc2)))

(defmethod expr_ :eand
  [glob-state name expr label-count]
  (let
    [lfalse (label-name name label-count)
     lend (label-name name (+ label-count 1))
     nlc (+ label-count 2)
     nlc1 (expr_ glob-state name (second expr) nlc)]
    (pop_ eax)
    (test_ eax eax)
    (je_ lfalse)
    (let
      [nlc2 (expr_ glob-state name (third expr) nlc1)]
      (pop_ eax)
      (test_ eax eax)
      (je_ lfalse)
      (push_ (const 1))
      (jmp_ lend)
      (label_ lfalse)
      (push_ (const 0))
      (label_ lend)
      nlc2)))

(defmethod expr_ :erel
  [glob-state name expr label-count]
  (let
    [nlc1 (expr_ glob-state name (second expr) label-count)
     nlc2 (expr_ glob-state name (fourth expr) nlc1)
     l1 (label-name name nlc2)
     l2 (label-name name (+ nlc2 1))
     op (match (third expr)
          [:lth] jg_
          [:le] jge_
          [:gth] jl_
          [:ge] jle_
          [:eq] je_
          [:ieq] jne_)]
    (pop_ edx)
    (pop_ eax)
    (cmp_ eax edx)
    (op l1)
    (push_ (const 0))
    (jmp_ l2)
    (label_ l1)
    (push_ (const 1))
    (label_ l2)
    (+ nlc2 2)))

(defmethod expr_ :emul
  [glob-state name expr label-count]
  (if (= (third expr) [:times])
    (let
      [nlc1 (expr_ glob-state name (second expr) label-count)
       nlc2 (expr_ glob-state name (fourth expr) nlc1)]
      (pop_ edx)
      (pop_ eax)
      (imul_ edx eax)
      (push_ eax)
      nlc2)
    (let
      [nlc1 (expr_ glob-state name (second expr) label-count)
       nlc2 (expr_ glob-state name (fourth expr) nlc1)]
      (pop_ ecx)
      (pop_ eax)
      (cdq_)
      (idiv_ ecx)
      (if (= (third expr) [:div])
        (push_ eax)
        (push_ edx))
      nlc2)))

(defmethod expr_ :eadd
  [glob-state name expr label-count]
  (let
    [lexpr (second expr)
     rexpr (fourth expr)
     op (if (= (third expr) [:plus])
          add_
          sub_)
     nlc1 (expr_ glob-state name lexpr label-count)
     nlc2 (expr_ glob-state name rexpr nlc1)]
    (pop_ edx)
    (pop_ eax)
    (op edx eax)
    (push_ eax)
    nlc2))

(defmethod expr_ :eclsapp
  [glob-state name expr label-count]
  (let
    [offset (second expr)
     args (reverse (third expr))
     nargs (count args)
     nlc (reduce #(expr_ glob-state name %2 %1)
           label-count args)]
    (pop_ eax)
    (push_ eax)
    (move_ (offset-addr offset eax) eax)
    (call_ (ptr eax))
    (add_ (const (* 4 nargs)) esp)
    (push_ eax)
    nlc))

(defmethod expr_ :eapp
  [glob-state name expr label-count]
  (let
    [ident (second (second expr))
     args (reverse (third expr))
     nargs (count args)
     nlc (reduce #(expr_ glob-state name %2 %1)
           label-count args)]
    (call_ ident)
    (add_ (const (* 4 nargs)) esp)
    (push_ eax)
    nlc))

(defmethod expr_ :earrlit
  [glob-state name expr label-count]
  (let
    [nlc1 (expr_ glob-state name (third expr) label-count)]
    (pop_ eax)
    (push_ eax)
    (imul_ (const 4) eax)
    (add_ (const 4) eax)
    (push_ eax)
    (call_ "malloc")
    (push_ eax)
    (call_ "_memSet")
    (pop_ eax)
    (pop_ ecx)
    (pop_ ecx)
    (push_ eax)
    (move_ ecx (offset-addr 0 eax))
    nlc1))

(defmethod expr_ :eclassinit
  [glob-state _ expr label-count]
  (let
    [type (second expr)
     clssdecl (second (find (.-classes glob-state) type))
     size (.-size clssdecl)
     funs (.-funs clssdecl)]
    (push_ (const (* 4 size)))
    (call_ "malloc")
    (push_ eax)
    (call_ "_memSet")
    (pop_ eax)
    (pop_ ecx)
    (doseq [def (vals funs)]
      (let
        [[fndef offset] def]
        (move_ (const (.-name fndef)) (offset-addr offset eax))))
    (push_ eax)
    label-count))

(defn- return_ []
  (leave_)
  (ret_)
  (println))

(defn- decls_
  [glob-state name type decls label-count]
  (reduce #(match %2
            [:init [:ident num] expr]
            (let
              [nlc (expr_ glob-state name expr %1)]
              (pop_ (offset-addr num ebp))
              nlc)
            [:noinit [:ident num]]
            (do
              (move_ (default-for-type type) (offset-addr num ebp))
              %1)) label-count decls))

(defmulti stmt_
  (fn
    [_ _ stmt _ ]
    (first stmt)))

(defmethod stmt_ :vret
  [_ _ _ label-count]
  (return_)
  label-count)

(defmethod stmt_ :ret
  [glob-state name stmt label-count]
  (let
    [nlc (expr_ glob-state name (second stmt) label-count)]
    (pop_ eax)
    (return_)
    nlc))

(defmethod stmt_ :block
  [glob-state name stmt label-count]
  (reduce #(stmt_ glob-state name %2 %1) label-count (rest stmt)))

(defmethod stmt_ :decl
  [glob-state name stmt label-count]
  (let [type (get-type stmt)]
    (decls_ glob-state name type (rest (rest stmt)) label-count)))

(defmethod stmt_ :ass
  [glob-state name stmt label-count]
  (let [expr (third stmt)
        nlc1 (expr_ glob-state name expr label-count)
        [nlc addr] (get-varnum glob-state name (second stmt) nlc1)]
    (pop_ edx)
    (move_ edx addr)
    nlc))

(defmethod stmt_ :cond
  [glob-state name stmt label-count]
  (let
    [nlc1 (expr_ glob-state name (second stmt) label-count)
     l1 (label-name name nlc1)]
    (pop_ eax)
    (test_ eax eax)
    (je_ l1)
    (let [nlc2 (stmt_ glob-state name (third stmt) (+ nlc1 1))]
      (label_ l1)
      nlc2)))

(defmethod stmt_ :condelse
  [glob-state name stmt label-count]
  (let
    [nlc1 (expr_ glob-state name (second stmt) label-count)
     l1 (label-name name nlc1)
     l2 (label-name name (+ nlc1 1))]
    (pop_ eax)
    (test_ eax eax)
    (je_ l1)
    (let [nlc2 (stmt_ glob-state name (third stmt) (+ nlc1 2))]
      (jmp_ l2)
      (label_ l1)
      (let [nlc3 (stmt_ glob-state name (fourth stmt) nlc2)]
        (label_ l2)
        nlc3))))

(defmethod stmt_ :while
  [glob-state name stmt label-count]
  (let
    [l1 (label-name name label-count)
     l2 (label-name name (+ label-count 1))]
    (jmp_ l2)
    (label_ l1)
    (let
      [nlc1 (stmt_ glob-state name (third stmt) (+ label-count 2))
       _ (label_ l2)
       nlc2 (expr_ glob-state name (second stmt) nlc1)]
      (pop_ eax)
      (test_ eax eax)
      (jne_ l1)
      nlc2)))

(defmethod stmt_ :sexp
  [glob-state name stmt label-count]
  (let
    [nlc (expr_ glob-state name (second stmt) label-count)]
    (pop_ eax)
    nlc))

(defmethod stmt_ :empty
  [_ _ _ label-count]
  label-count)

(defn- function_
  [glob-state fun]
  (let
    [[_ [nargs _] nstrings strings] (second (find (meta fun) "_vars"))
     [_ type [_ name] _ block] fun]
    (strings_ name strings nstrings)
    (println (str "\t" ".globl" "\t" name))
    (println (str "\t" ".type" "\t" name ", @function"))
    (println (str name ":"))
    (push_ ebp)
    (move_ esp ebp)
    (sub_ (const (* 4 (+ 1 nargs))) esp)

    (stmt_ glob-state name block 0)
    (if (= type [:void])
      (return_))))

(defn asm-compile
  [glob-state tree]
  (empty-string_)
  (doseq [fn tree] (function_ glob-state fn)))
