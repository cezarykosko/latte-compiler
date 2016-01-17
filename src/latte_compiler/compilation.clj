(ns latte-compiler.compilation
  (:require [latte-compiler.util :as util]
            [clojure.core.match :as match]))

(defn- third
  [coll]
  (first (next (next coll))))

(defn- fourth
  [coll]
  (first (next (next (next coll)))))

(defn- const
  [name]
  (str "$" name))

(def empty-string "emptystring")

(defn- get-type
  [obj]
  (second (find (meta obj) "_type")))

(defn- default-for-type
  [type]
  (match/match type
    [:int] (const 0)
    [:string] (const empty-string)
    [:bool] (const 0)
    ))

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

(defn- cdq_
  []
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

(defn- leave_
  []
  (println (str "\t" "leave")))

(defn- ret_
  []
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

(defn- empty-string_
  []
  (println (str empty-string ":"))
  (println (str "\t" ".string \"" "\""))
  (println (str "\t" ".text"))
  (println)
  )

(defn- strings_
  [name strings nstrings]
  (doseq [n (range 1 (+ nstrings 1))]
    (println (str (string-addr name n) ":"))
    (println (str "\t" ".string \"" (second (find strings n)) "\""))
    (println (str "\t" ".text"))
    (println)
    ))

(defn- expr_
  [name expr label-count]
  (match/match (first expr)
    :elittrue (do
                (push_ (const 1))
                label-count)
    :elitfalse (do
                 (push_ (const 0))
                 label-count)
    :elitint (do
               (push_ (const (second expr)))
               label-count)
    :estring (do
               (push_ (const (string-addr name (second expr))))
               label-count)
    :evar (let
            [num (second (second expr))]
            (move_ (offset-addr num ebp) eax)
            (push_ eax)
            label-count
            )
    :neg (let
           [nlc (expr_ name (second expr) label-count)]
           (pop_ edx)
           (move_ (const 0) eax)
           (sub_ edx eax)
           (push_ eax)
           nlc
           )
    :not (let
           [nlc (expr_ name (second expr) label-count)]
           (pop_ eax)
           (xor_ (const 1) eax)
           (push_ eax)
           nlc
           )
    :eor (let
           [ltrue (label-name name label-count)
            lend (label-name name (+ label-count 1))
            nlc (+ label-count 2)
            nlc1 (expr_ name (second expr) nlc)]
           (pop_ eax)
           (test_ eax eax)
           (jne_ ltrue)
           (let
             [nlc2 (expr_ name (third expr) nlc1)]
             (pop_ eax)
             (test_ eax eax)
             (jne_ ltrue)
             (push_ (const 0))
             (jmp_ lend)
             (label_ ltrue)
             (push_ (const 1))
             (label_ lend)
             nlc2
             )
           )
    :eand (let
            [lfalse (label-name name label-count)
             lend (label-name name (+ label-count 1))
             nlc (+ label-count 2)
             nlc1 (expr_ name (second expr) nlc)]
            (pop_ eax)
            (test_ eax eax)
            (je_ lfalse)
            (let
              [nlc2 (expr_ name (third expr) nlc1)]
              (pop_ eax)
              (test_ eax eax)
              (je_ lfalse)
              (push_ (const 1))
              (jmp_ lend)
              (label_ lfalse)
              (push_ (const 0))
              (label_ lend)
              nlc2
              )
            )
    :erel (let
            [nlc1 (expr_ name (second expr) label-count)
             nlc2 (expr_ name (fourth expr) nlc1)
             l1 (label-name name nlc2)
             l2 (label-name name (+ nlc2 1))
             op (match/match (third expr)
                  [:lth] jg_
                  [:le] jge_
                  [:gth] jl_
                  [:ge] jle_
                  [:eq] je_
                  [:ieq] jne_)
             ]
            (pop_ edx)
            (pop_ eax)
            (cmp_ eax edx)
            (op l1)
            (push_ (const 0))
            (jmp_ l2)
            (label_ l1)
            (push_ (const 1))
            (label_ l2)
            (+ nlc2 2)
            )
    :emul (if (= (third expr) [:times])
            (let
              [nlc1 (expr_ name (second expr) label-count)
               nlc2 (expr_ name (fourth expr) nlc1)]
              (pop_ edx)
              (pop_ eax)
              (imul_ edx eax)
              (push_ eax)
              nlc2
              )
            (let
              [nlc1 (expr_ name (second expr) label-count)
               nlc2 (expr_ name (fourth expr) nlc1)]
              (pop_ ecx)
              (pop_ eax)
              (cdq_)
              (idiv_ ecx)
              (if (= (third expr) [:div])
                (push_ eax)
                (push_ edx)
                )
              nlc2))
    :eadd (let
            [lexpr (second expr)
             rexpr (fourth expr)
             op (if (= (third expr) [:plus])
                  add_
                  sub_
                  )
             nlc1 (expr_ name lexpr label-count)
             nlc2 (expr_ name rexpr nlc1)
             ]
            (pop_ edx)
            (pop_ eax)
            (op edx eax)
            (push_ eax)
            nlc2
            )
    :eapp (let
            [ident (second (second expr))
             args (reverse (third expr))
             nargs (count args)
             nlc (reduce #(expr_ name %2 %1)
                   label-count args)]
            (call_ ident)
            (add_ (const (* 4 nargs)) esp)
            (push_ eax)
            nlc
            )
    ))

(defn- return_
  []
  (leave_)
  (ret_)
  (println)
  )

(defn- decls_
  [name type decls label-count]
  (reduce #(match/match %2
            [:init [:ident num] expr]
            (let
              [nlc (expr_ name expr %1)]
              (pop_ (offset-addr num ebp))
              nlc)
            [:noinit [:ident num]]
            (do
              (move_ (default-for-type type) (offset-addr num ebp))
              %1)) label-count decls))

(defn- stmt_
  [name stmt label-count]
  (let [type (get-type stmt)]
    (match/match (first stmt)
      :vret (do
              (return_)
              label-count)
      :ret (let
             [nlc (expr_ name (second stmt) label-count)]
             (pop_ eax)
             (return_)
             nlc
             )
      :block (reduce #(stmt_ name %2 %1) label-count (rest stmt))
      :incr (let [num (second (second stmt))]
              (add_ (const 1) (offset-addr num ebp))
              label-count)
      :decr (let [num (second (second stmt))]
              (sub_ (const 1) (offset-addr num ebp))
              label-count)
      :decl (do
              (decls_ name type (rest (rest stmt)) label-count))
      :ass (let [ident (second (second stmt))
                 expr (third stmt)
                 nlc (expr_ name expr label-count)]
             (pop_ (offset-addr ident ebp))
             nlc
             )
      :cond (let
              [nlc1 (expr_ name (second stmt) label-count)
               l1 (label-name name nlc1)]
              (pop_ eax)
              (test_ eax eax)
              (je_ l1)
              (let [nlc2 (stmt_ name (third stmt) (+ nlc1 1))]
                (label_ l1)
                nlc2))
      :condelse (let
                  [nlc1 (expr_ name (second stmt) label-count)
                   l1 (label-name name nlc1)
                   l2 (label-name name (+ nlc1 1))]
                  (pop_ eax)
                  (test_ eax eax)
                  (je_ l1)
                  (let [nlc2 (stmt_ name (third stmt) (+ nlc1 2))]
                    (jmp_ l2)
                    (label_ l1)
                    (let [nlc3 (stmt_ name (fourth stmt) nlc2)]
                      (label_ l2)
                      nlc3
                      )))
      :while (let
               [l1 (label-name name label-count)
                l2 (label-name name (+ label-count 1))]
               (jmp_ l2)
               (label_ l1)
               (let
                 [nlc1 (stmt_ name (third stmt) (+ label-count 2))
                  _ (label_ l2)
                  nlc2 (expr_ name (second stmt) nlc1)]
                 (pop_ eax)
                 (test_ eax eax)
                 (jne_ l1)
                 nlc2
                 ))
      :sexp (let
              [nlc (expr_ name (second stmt) label-count)]
              (pop_ eax)
              nlc)
      :empty label-count
      ))
  )

(defn- function_
  [fun]
  (let
    [[_ nargs nstrings strings] (second (find (meta fun) "_vars"))
     [:fndef type [_ name] args block] fun
     ]
    (strings_ name strings nstrings)
    (println (str "\t" ".globl" "\t" name))
    (println (str "\t" ".type" "\t" name ", @function"))
    (println (str name ":"))
    (push_ ebp)
    (move_ esp ebp)
    (sub_ (const (* 4 (+ 1 nargs))) esp)

    (stmt_ name block 0)
    (if (= type [:void])
      (return_))
    ))


(defn asm-compile
  [tree]
  (empty-string_)
  (doseq [fn tree] (function_ fn))
  )
