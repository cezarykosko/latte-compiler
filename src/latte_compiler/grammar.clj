(ns latte-compiler.grammar
  (:require [instaparse.core :as insta]
            [clojure.edn]
            [latte-compiler.util :as util]))

(def FILENAME
  "latte.bnf")

(def FILE
  (clojure.java.io/resource FILENAME))

(def latte_parser
  (insta/parser FILE :start :program))

(defn wrap
  [name val]
  [name val])

(def intify
  #(->> %& str clojure.edn/read-string first (wrap :elitint)))

(def stringify
  #(->> %& (map str) (apply str) (wrap :ident)))

(def latte
  #(->> (latte_parser %)
        (insta/add-line-and-column-info-to-metadata %)
        (insta/transform
          {
           :ident   stringify
           :elitint intify
           })))

(defn parse
  [code]
  (let [parse (latte code)]
    (if (insta/failure? parse)
      [:err (insta/get-failure parse)]
      [:succ parse]
      )
    )
  )
