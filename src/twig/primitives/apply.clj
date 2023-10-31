(ns twig.primitives.apply
  (:require [twig.types :as types]
            [twig.syntax :as syntax]
            [twig.primitives.base :as primitives]
            [twig.unification :as unification]
            [twig.validation :as validation]
            [twig.evaluation :as evaluation]
            [twig.print :as print]
            [clojure.string :as string]))

(defn validate-function-args [env a function args expr]
  (let
   [signature (types/descheme (:type function))
    step (fn [validated substitutions type args]
           (let
            [parameters (:parameters type)
             body (:body type)]
             (if (empty? args)
               (do
                 (assert (empty? parameters))
                 (assoc expr
                        :valid? true
                        :elements (concat [(assoc a :type (types/primitive-type :syntax)) function] (reverse validated))
                        :type body
                        :variables (apply concat (map :variables validated))
                        :substitutions substitutions
                        :environment env))
               (let
                [arg (validation/validate (unification/substitute-environment substitutions env) (first args))]
                 (if (not (:valid? arg))
                   arg
                   (let
                    [argsub (:substitutions arg)
                     return (types/function-type (rest parameters) body)
                     signature (types/function-type [(first parameters)] return)
                     compare (types/function-type [(:type arg)] (types/variable-type))
                     appsub (unification/most-general-unifier (unification/substitute argsub signature)
                                                              compare)]
                     (if appsub
                       (let
                        [substitutions (concat appsub argsub substitutions)]
                         (recur (cons (assoc arg :type (unification/substitute substitutions (:type arg)))
                                      validated)
                                substitutions
                                (unification/substitute substitutions return)
                                (rest args)))
                       (validation/invalid [(str "could not unify: "
                                                 (print/format-syntax 0 arg) " "
                                                 (print/bracket :round (str ": " (print/format-type (first parameters))))
                                                 "\nin: "
                                                 (syntax/origin-str (:origin expr)))]))))))))]
    (step [] (:substitutions function) signature args)))

(defn validate-apply [env a args expr]
  (let
   [function (validation/validate env (first args))
    args (rest args)
    type (:type function)
    core (types/descheme type)]
    (if (types/function? core)
      (if (not (= (count (:parameters core))
                  (count args)))
        (validation/invalid ["arity error"])
        (validate-function-args env a function args expr))
      (if (types/variable? core)
        (let
         [params (map (fn [_] (types/variable-type)) args)
          body (types/variable-type)
          signature (types/scheme-type (concat params [body]) (types/function-type params body))
          substitutions [(unification/substitution signature core)]
          function (assoc function
                          :type signature
                          :substitutions substitutions)]
          (validate-function-args env a function args expr))
        (validation/invalid ["invalid application"])))))

(defn eval-apply [env args _]
  (let
   [function (evaluation/evaluate env (first args))]
    ((:evaluate function) (map (partial evaluation/evaluate env) (rest args)))))

(defn print-apply [indentation expr]
  (print/annotate (:type expr)
                  (:substitutions expr)
                  (str "\n" (print/pad (+ 3 indentation))
                       (print/bracket :round (string/join (str "\n" (print/pad (+ 4 indentation)))
                                                          (map (partial print/format-syntax (+ 4 indentation)) (:elements expr)))))))

(def apply-syntax
  (primitives/syntax
   validate-apply
   eval-apply
   print-apply))
