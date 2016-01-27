(ns latte-compiler.static-analysis-test
  (:require
    [clojure.test :refer :all]
    [latte-compiler.static-analysis :as static-analysis]
    [latte-compiler.state :as state]))

(deftest ext-name
   (testing "whether the extension name is correct")
   (is (nil? (static-analysis/class-dep [:noextclssdef [:ident "Node"] [:clssdecls]])))
   (is (= (static-analysis/class-dep [:extclssdef [:ident "Rectangle"] [:ident "Shape"]]) [:ident "Shape"]))
)

(deftest name
  (testing "whether the name is correct")
  (is (= (static-analysis/class-name [:noextclssdef [:ident "Node"] [:clssdecls]]) [:ident "Node"]))
  )

(deftest mapfun
  (testing "fun mapping")
  (is (= (static-analysis/map-fun [:fndef [:void] [:ident "neenene"] [:args [:arg [:int] [:ident "rzecz"]]] [:block]])
         [[:ident "neenene"] (state/->FunDef [:ident "neenene"] [:void] [[:int]])]))
  (is (= (static-analysis/map-fun [:fndef [:void] [:ident "neenene"] [:args] [:block]])
         [[:ident "neenene"] (state/->FunDef [:ident "neenene"] [:void] [])]))
  )
