(ns twig.primitives.logic
  (:require [twig.types :as types]
            [twig.values :as values]
            [twig.primitives.base :as primitives]
            [twig.unification :as unification]
            [twig.validation :as validation]
            [twig.evaluation :as evaluation]))

(defn validate-and-or [to-type]
  (fn [env a args expr]
    (let
     [[valid? validated substitutions] (validation/validate-values env args)]
      (if (not valid?)
        validated
        (assoc expr
               :valid? true
               :elements (cons (assoc a :type (types/primitive-type :syntax)) validated)
               :type (to-type (map :type validated))
               :variables (apply concat (map :variables validated))
               :substitutions substitutions
               :environment (unification/substitute-environment substitutions env))))))

(defn eval-and [env args _]
  (reduce (fn [b a] (if (values/equals-false? b)
                      b
                      (evaluation/evaluate env a)))
          values/true-atom
          args))

(def and-syntax
  (primitives/syntax
   (validate-and-or (fn [args]
                      (if (empty? args)
                        types/type-true
                        (types/to-union (cons types/type-false args)))))
   eval-and
   primitives/default-print))

(defn eval-or [env args _]
  (reduce (fn [b a] (if (values/equals-false? b)
                      (evaluation/evaluate env a)
                      b))
          values/false-atom
          args))

(def or-syntax
  (primitives/syntax
   (validate-and-or (fn [args]
                      (types/to-union (cons types/type-false args))))
   eval-or
   primitives/default-print))

(defn validate-if [env a args expr]
  (if (not (= 3 (count args)))
    (validation/invalid ["malformed if statement"])
    (let
     [[valid? validated substitutions] (validation/validate-values env args)]
      (if (not valid?)
        validated
        (assoc expr
               :valid? true
               :elements (cons (assoc a :type (types/primitive-type :syntax)) validated)
               :type (types/to-union (map :type (rest validated)))
               :variables (apply concat (map :variables validated))
               :substitutions substitutions
               :environment (unification/substitute-environment substitutions env))))))

(defn eval-if [env args _]
  (let
   [[a b c] args]
    (evaluation/evaluate env
                         (if (not (values/equals-false? (evaluation/evaluate env a)))
                           b
                           c))))

(def if-syntax
  (primitives/syntax
   validate-if
   eval-if
   primitives/default-print))
