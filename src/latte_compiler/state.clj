(ns latte-compiler.state)

(defrecord ClassDef [fields funs size parents])
(defrecord FunDef [name outType inTypes])
(defrecord FieldDef [name type])
(defrecord GlobState [cls-parents classes funs std-types])

(def default-state
  (->GlobState
    (hash-map)
    (hash-map
      [:ident "_arr"] (->ClassDef '({[:ident "length"] [[:int] 0]}) {} 0 '([:ident "_arr"]))
      )
    (hash-map
      [:ident "printInt"] (->FunDef [:ident "printInt"] [:void] [[:int]])
      [:ident "printString"] (->FunDef [:ident "printString"] [:void] [[:string]])
      [:ident "error"] (->FunDef [:ident "error"] [:void] [])
      [:ident "readInt"] (->FunDef [:ident "readInt"] [:int] [])
      [:ident "readString"] (->FunDef [:ident "readString"] [:string] [])
      [:ident "malloc"] (->FunDef [:ident "malloc"] [:int] [:int])
      )
    (hash-set :void :int :string :boolean)
    ))
