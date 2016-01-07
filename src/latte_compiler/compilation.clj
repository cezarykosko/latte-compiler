(ns latte-compiler.compilation
  (:require [latte-compiler.util :as util]
            [clojure.core.match :as match]))

(defn third
  [coll]
  (first (next (next coll))))

(defn fourth
  [coll]
  (first (next (next (next coll)))))

(defn const
  [name]
  (str "$" name))

(def empty-string "empty-string")

(defn get-type
  [obj]
  (second (find (meta obj) "_type")))

(defn default-for-type
  [type]
  (match/match type
    [:int] (const 0)
    [:string] (const empty-string)
    [:bool] (const 0)
    ))

(defn label-name
  [name lcount]
  (str "." name "-label-" lcount))

(defn print-label
  [label]
  (println (str label ":")))

(defn type-suffix
  [type]
  (match/match type
    [:int] "l"
    [:string] "l"
    [:bool] "b"
    :else "l"
    ))

(defn pop_
  [type dest]
  (println (str "\t" "pop" (type-suffix type) " " dest)))

(defn push_
  [type src]
  (println (str "\t" "push" (type-suffix type) " " src)))

(defn move_
  [type src dest]
  (println (str "\t" "mov" (type-suffix type) " " src ", " dest)))

(defn string-addr
  [name n]
  (str name "-string-" n))


(defn offset-addr
  [offset addr]
  (if (= offset 0)
    (str "(" addr ")")
    (str (* 4 offset) "(" addr ")")))

(def eax "%eax")
(def ebx "%ebx")
(def ecx "%ecx")
(def edx "%edx")
(def esi "%esi")
(def edi "%edi")
(def ebp "%ebp")
(def esp "%esp")

(defn print-empty-string
  []
  (println (str empty-string ":"))
  (println (str "\t" ".string \"" "\""))
  (println (str "\t" ".text"))
  (println)
  )

(defn print-strings
  [name strings nstrings]
  (doseq [n (range 1 (+ nstrings 1))]
    (println (str (string-addr name n) ":"))
    (println (str "\t" ".string \"" (second (find strings n)) "\""))
    (println (str "\t" ".text"))
    (println)
    ))

(defn print-expr
  [name expr label-count]
  (let [type (get-type expr)]
    (match/match (first expr)
      :elittrue (do
                  (push_ type (const 1))
                  label-count)
      :elitfalse (do
                   (push_ type (const 0))
                   label-count)
      :elitint (do
                 (push_ type (const (second expr)))
                 label-count)
      :estring (do
                 (push_ type (const (string-addr name (second expr))))
                 label-count)
      :evar (let
              [num (second (second expr))]
              (push_ type (offset-addr (* 4 num) ebp))
              label-count
              )
      :neg (let
             [nlc (print-expr name (second expr) label-count)]
             (pop_ type eax)
             (println (str "\tsub" (type-suffix type) "\t" (const 0) ", " eax))
             (push_ type eax)
             nlc
             )
      :not (let
             [nlc (print-expr name (second expr) label-count)]
             (pop_ type eax)
             (println (str "\txor" (type-suffix type) "\t" (const 1) ", " eax))
             (push_ type eax)
             nlc
             )
      :eor (let
             [nlc1 (print-expr name (second expr) label-count)
              nlc2 (print-expr name (third expr) nlc1)]
             (pop_ type edx)
             (pop_ type eax)
             (println (str "\tor" (type-suffix type) "\t" edx ", " eax))
             (push_ type eax)
             nlc2
             )
      :eand (let
              [nlc1 (print-expr name (second expr) label-count)
               nlc2 (print-expr name (third expr) nlc1)]
              (pop_ type edx)
              (pop_ type eax)
              (println (str "\tand" (type-suffix type) "\t" edx ", " eax))
              (push_ type eax)
              nlc2
              )
      :erel (let
              [nlc1 (print-expr name (second expr) label-count)
               nlc2 (print-expr name (fourth expr) nlc1)
               l1 (label-name name nlc2)
               l2 (label-name name (+ nlc2 1))]
              (pop_ type edx)
              (pop_ type eax)
              (println (str "\tcmp" (type-suffix type) "\t" eax ", " edx))
              (println (str "\t" (match/match (third expr)
                                   [:lth] "jl"
                                   [:le] "jle"
                                   [:gth] "jg"
                                   [:ge] "jge"
                                   [:eq] "je"
                                   [:ieq] "jne"
                                   ) "\t" l1))
              (push_ type (const 0))
              (print-label l1)
              (push_ type (const 1))
              (print-label l2)
              (+ nlc2 2)
              )
      :emul (if (= (third expr) [:times])
              (let
                [nlc1 (print-expr name (second expr) label-count)
                 nlc2 (print-expr name (third expr) nlc1)]
                (pop_ type edx)
                (pop_ type eax)
                (println (str "\timul" (type-suffix type) "\t" edx ", " eax))
                (push_ type eax)
                nlc2
                )
              (let
                [nlc1 (print-expr name (second expr) label-count)
                 nlc2 (print-expr name (third expr) nlc1)]
                (pop_ type ecx)
                (pop_ type eax)
                (println "\tcdq")
                (println (str "\tidiv" (type-suffix type) "\t" ecx))
                (if (= (third expr) [:div])
                  (push_ type eax)
                  (push_ type edx)
                  )
                nlc2))
      :eadd (let
              [lexpr (second expr)
               rexpr (fourth expr)
               op (if (= (third expr) [:plus])
                    "add"
                    "sub"
                    )
               nlc1 (print-expr name lexpr label-count)
               nlc2 (print-expr name rexpr nlc1)
               ]
              (pop_ type edx)
              (pop_ type eax)
              (println (str "\t" op (type-suffix type) "\t" edx ", " eax))
              (push_ type eax)
              nlc2
              )
      :eapp (let
              [ident (second (second expr))
               args (reverse (rest (rest expr)))
               ]
              (reduce #(do
                        (push_ nil ebx)
                        (move_ nil esp ebx)
                        (print-expr name %2 %1)
                        (println (str "\tcall " ident))
                        (move_ nil ebx esp)
                        (pop_ nil ebx)
                        (push_ type eax))
                label-count args
                )
              )
      )))

(defn print-return
  []
  (println (str "\t" "leave"))
  (println (str "\t" "ret"))
  (println)
  )

(defn print-decls
  [name type decls label-count]
  (reduce #(match/match %2
            [:init [:ident num] expr]
            (let
              [nlc (print-expr name expr %1)]
              (pop_ type (offset-addr (* 4 num) ebp))
              nlc)
            [:noinit [:ident num]]
            (do
              (move_ type (default-for-type type) (offset-addr (* 4 num) ebp))
              %1)) label-count decls))

(defn print-stmt
  [name stmt label-count]
  (let [type (get-type stmt)]
    (match/match (first stmt)
      :vret (do
              (print-return)
              label-count)
      :ret (let
             [nlc (print-expr name (second stmt) label-count)]
             (pop_ (get-type stmt) eax)
             (print-return)
             nlc
             )
      :block (reduce #(print-stmt name %2 %1) label-count (rest stmt))
      :incr (let [num (second (second stmt))]
              (println (str "\tadd" (type-suffix type) (const 1) (offset-addr (* 4 num) ebp)))
              label-count)
      :decr (let [num (second (second stmt))]
              (println (str "\tadd" (type-suffix type) (const 1) (offset-addr (* 4 num) ebp)))
              label-count)
      :decl (do
              (print-decls name type (rest (rest stmt)) label-count))
      :ass (let [ident (second (second stmt))
                 expr (third stmt)
                 nlc (print-expr name expr label-count)]
             (pop_ type (offset-addr (* 4 ident) ebp))
             nlc
             )
      :cond (let
              [nlc1 (print-expr name (second stmt) label-count)
               l1 (label-name name nlc1)]
              (pop_ type eax)
              (println (str "\ttest\t" eax ", " eax))
              (println (str "\tje\t" l1))
              (let [nlc2 (print-stmt name (third stmt) (+ nlc1 1))]
                (print-label l1)
                nlc2))
      :condelse (let
                  [nlc1 (print-expr name (second stmt) label-count)
                   l1 (label-name name nlc1)
                   l2 (label-name name (+ nlc1 1))]
                  (pop_ type eax)
                  (println (str "\ttest\t" eax ", " eax))
                  (println (str "\tje\t" l1))
                  (let [nlc2 (print-stmt name (third stmt) (+ nlc1 2))]
                    (println (str "\tjmp\t" l2))
                    (print-label l1)
                    (let [nlc3 (print-stmt name (fourth stmt) nlc2)]
                      (print-label l2)
                      nlc3
                      )))
      :while (let
               [l1 (label-name name label-count)
                l2 (label-name name (+ label-count 1))]
               (println (str "\tjmp\t" l2))
               (print-label l1)
               (let
                 [nlc1 (print-stmt name (third stmt) (+ label-count 2))
                  nlc2 (do (print-label l2) (print-expr label-name (second stmt) nlc1))]
                 (pop_ type eax)
                 (println (str "\ttest\t" eax ", " eax))
                 (println (str "\tjne\t" l1))
                 nlc2
                 ))
      :sexp (let
              [nlc (print-expr name (second stmt) label-count)]
              (pop_ type eax)
              nlc)
      ))
  )

(defn print-function
  [fun]
  (let
    [[_ nargs nstrings strings] (second (find (meta fun) "_vars"))
     [:fndef type [_ name] args block] fun
     ]
    (print-strings name strings nstrings)
    (println (str "\t" ".globl" "\t" name))
    (println (str "\t" ".type" "\t" name ", @function"))
    (println (str name ":"))
    (println (str "\t" "pushl" "\t" ebp))
    (println (str "\t" "movl" "\t" esp ", " ebp))
    (println (str "\t" "subl" "\t" (const (* 4 nargs)) ", " esp))

    (print-stmt name block 0)

    ))


(defn asm-compile
  [tree]
  (print-empty-string)
  (doseq [fn tree] (print-function fn))
  )
