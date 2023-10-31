(ns twig.primitives.sequences
  (:require [twig.types :as types]
            [twig.primitives.base :as primitives]
            [twig.unification :as unification]
            [twig.validation :as validation]
            [twig.evaluation :as evaluation]
            [twig.values :as values]))

(defn validate-link [env a args expr]
  (if (not (= 2 (count args)))
    (validation/invalid ["malformed link"])
    (let
     [[valid? validated substitutions] (validation/validate-values env args)]
      (if (not valid?)
        validated
        (let
         [[first rest] validated
          seqtype (:type rest)
          var (types/variable-type)
          scheme (types/scheme-type [var] (types/sequence-type var))
          seqsubs (unification/most-general-unifier scheme seqtype)]
          (if (not seqsubs)
            (validation/invalid ["malformed link"])
            (let
             [substitutions (concat seqsubs substitutions)
              seqtype (unification/substitute substitutions seqtype)]
              (assoc expr
                     :valid? true
                     :elements [(assoc a :type (types/primitive-type :syntax)) first (assoc rest :type seqtype)]
                     :type (types/sequence-type (types/to-union (cons (:type first) (types/union-value (types/sequence-value seqtype)))))
                     :variables (unification/remaining-variables seqsubs [var])
                     :substitutions substitutions
                     :environment (unification/substitute-environment substitutions env)))))))))

(defn eval-link [env args expr]
  (let
   [[first rest] (map (partial evaluation/evaluate env) (:elements args))]
    (values/to-sequence (:type expr) (cons first (:elements rest)))))

(def link-syntax
  (primitives/syntax
   validate-link
   eval-link
   primitives/default-print))
