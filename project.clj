(defproject latte-compiler "1.0"
  :description "A clojure-based compiler of Latte programming language"
  :url "http://github.com/cezarykosko/latte-compiler"
  :main latte-compiler.core

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/algo.monads "0.1.5"]
                 [instaparse "1.4.1"]]

  :profiles {:uberjar {:aot :all}}
  :javac-options ["-Dclojure.compiler.direct-linking=true -XX:+AggressiveOpts -XX:-UseCompressedOops"]
  :jvm-opts ^:replace ["-Dclojure.compiler.direct-linking=true  -XX:MaxInlineLevel=20 -XX:+AggressiveOpts"]
  )
