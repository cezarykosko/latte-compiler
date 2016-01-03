(defproject latte-compiler "0.1.0-SNAPSHOT"
  :description "A clojure-based compiler of Latte programming language"
  :url "http://github.com/cezarykosko/latte-compiler"
  :main latte-compiler.core

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/algo.monads "0.1.5"]
                 [instaparse "1.4.1"]])
