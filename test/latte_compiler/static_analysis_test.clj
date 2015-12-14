(ns latte-compiler.static-analysis-test
  (:require
    [clojure.test :refer :all]
    [latte-compiler.static-analysis :as static-analysis]))

(deftest ext-name
   (testing "whether the extension name is correct")
   (is (nil? (static-analysis/class-dep [:noextclssdef [:ident "Node"] [:clssdecls]])))
   (is (= (static-analysis/class-dep [:extclssdef [:ident "Rectangle"] [:tident [:ident "Shape"]]]) [:ident "Shape"]))
)

(deftest name
  (testing "whether the name is correct")
  (is (= (static-analysis/class-name [:noextclssdef [:ident "Node"] [:clssdecls]]) [:ident "Node"]))
  )

(deftest maptype
  (testing "type mapping")
  (is (= (static-analysis/map-type [:tident [:ident "Ololo"]]) [:ident "Ololo"]))
  (is (= (static-analysis/map-type [:ident "Ololo"]) [:ident "Ololo"]))
  (is (= (static-analysis/map-type [:int]) :int))
  )

(deftest maparg
  (testing "arg mapping")
  (is (= (static-analysis/map-arg [:arg [:tident [:ident "Ololo"]] [:ident "dsada"]]) [:ident "Ololo"]))
  )

(deftest mapfun
  (testing "fun mapping")
  (is (= (static-analysis/map-fun [:fndef [:void] [:ident "neenene"] [:args [:arg [:int] [:ident "rzecz"]]] [:block]])
         ["neenene" (static-analysis/->FunDef "neenene" :void [:int])]))
  (is (= (static-analysis/map-fun [:fndef [:void] [:ident "neenene"] [:args] [:block]])
         ["neenene" (static-analysis/->FunDef "neenene" :void [])]))
  )
