(ns twig.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [twig.read :as twig-read]
            [twig.types :as types]
            [twig.values :as values]
            [twig.errors :as errors]
            [twig.validation :as validate]
            [twig.unification :as unification]
            [twig.evaluation :as evaluation]
            [twig.print :as print]
            [twig.default-env :as env]))

(defn validate [a] (validate/validate env/default a))
(defn eval-expr [a] (evaluation/evaluate env/default a))
(defn read-str [str] (let
                      [parsed (twig-read/parse-tokens str 0 [] [] str)]
                       (and (= 1 (count parsed))
                            (first parsed))))

(def unify unification/most-general-unifier)

(defn test-type [str expected]
  (testing str
    (let
     [expr (read-str str)
      val (validate expr)]
      (is (and (:valid? val)
               (= expected (:type val)))
          [expected (:type val)]))))

(defn test-expr [str expected]
  (testing str
    (let
     [expr (read-str str)
      val (validate expr)]
      (is (and (:valid? val)
               (= expected (eval-expr val)))
          [(print/annotate (:type expected) [] (:value expected))
           (and (:valid? val) (let [result (eval-expr val)]
                                (print/annotate (:type result) [] (:value result))))]))))

(defn test-validation [str expected]
  (testing str
    (is (= expected (validate (read-str str))))))

(deftest empty-expr (test-validation "()" (validate/invalid errors/empty-expression)))

(deftest true-literal (test-expr "#t" values/true-atom))

(deftest literals (test-expr "[#f #t]" (values/to-sequence (types/sequence-type types/type-boolean) [values/false-atom values/true-atom])))

(deftest vector-type (test-type "[#f 2 \"str\"]" (types/sequence-type (types/to-union [types/type-boolean types/type-number types/type-string]))))

(deftest env (test-expr "false" values/false-atom))

(deftest and-syntax (test-expr "(and #t 2 3)" (values/to-atom types/type-number 3)))
(deftest or-syntax (test-expr "(or 1 2)" (values/to-atom types/type-number 1)))
(deftest if-syntax (test-expr "(if #f (and) (if 2 3 4))" (values/to-atom types/type-number 3)))
(deftest add-syntax (test-expr "(+ 1 2)" (values/to-atom types/type-number 3)))
(deftest negative-syntax (test-expr "(- 1)" (values/to-atom types/type-number -1)))
(deftest add-type (test-type "(+ 1 2)" types/type-number))
;(deftest incorrect-add (test-validation "(+ \"str\" 2)" (validate/invalid (errors/type-mismatch [types/type-number types/type-number] [types/type-string types/type-number]))))
(deftest lambda-type (test-type "(fn (a b) (+ a b))" (types/function-type [types/type-number types/type-number] types/type-number)))

(deftest unify-equal (testing "unify" (is (= [] (unify types/type-number types/type-number)))))
(deftest unify-inequal (testing "unify" (is (= false (unify types/type-string types/type-number)))))
(deftest unify-variable (testing "unify" (let
                                          [a types/type-number
                                           b (types/variable-type)]
                                           (is (= [(unification/substitution a b)]
                                                  (unify a b)
                                                  (unify b a))))))

(deftest unify-functions (testing "unify" (let
                                           [aparam [(types/variable-type) (types/variable-type)]
                                            bparam [types/type-number types/type-string]
                                            abody (types/variable-type)
                                            bbody types/type-symbol]
                                            (is (= [(unification/substitution bbody abody)
                                                    (unification/substitution (second bparam) (second aparam))
                                                    (unification/substitution (first bparam) (first aparam))]
                                                   (unify (types/function-type aparam abody)
                                                          (types/function-type bparam bbody)))))))
