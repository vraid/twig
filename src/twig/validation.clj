(ns twig.validation
  (:require [twig.types :as types]
            [twig.errors :as errors]
            [twig.syntax :as syntax]
            [twig.unification :as unification]
            [twig.primitives.base :as primitives]))

(defn invalid [errors]
  {:valid? false
   :errors errors})

(defn all-valid? [ls]
  (reduce (fn [b a] (and b (:valid? a))) true ls))

(defn join-errors [ls]
  (invalid (apply concat (map :errors ls))))

(defn with-type [type expr]
  (assoc expr
         :type type
         :variables (types/scheme-variables type)))

(defn validate-literal [_ a]
  (assoc a :valid? true))

(declare validate-base)
(declare validate)

(defn instantiate-scheme [a]
  (let
   [var (:variables a)
    subs (map (fn [a] (unification/substitution (types/variable-type) a)) var)
    inst (map :replacement subs)]
    (assoc a
           :variables inst
           :value (unification/substitute subs (:value a)))))

(defn validate-values [env values]
  (let
   [f (fn [validated env substitutions ls]
        (if (empty? ls)
          [true (reverse validated) substitutions]
          (let
           [a (validate env (first ls))]
            (if (not (:valid? a))
              [false a []]
              (let
               [substitutions (concat (:substitutions a) substitutions)]
                (recur (cons a validated) (unification/substitute-environment substitutions env) substitutions (rest ls)))))))]
    (f [] env [] values)))

(defn validate-symbol [accept-syntax? env {:keys [value] :as a}]
  (if (contains? env value)
    (let
     [binding (env value)
      t (:type binding)
      t (if (and (not (= :syntax t))
                 (types/scheme? t)) (instantiate-scheme t) t)]
      (if (and (= :syntax t) (not accept-syntax?))
        (invalid ["misplaced syntax"])
        (assoc a
               :valid? true
               :type t
               :environment env)))
    (invalid (errors/undefined-variable value))))

(defn validate-call [env a args expr]
  (assert (= :syntax (:internal-type a)))
  (let
   [[a args] (if (= :syntax (:type a))
               [a args]
               [(validate-symbol true env (syntax/syntax-symbol (:origin expr) primitives/default-syntax))
                (cons a args)])]
    ((:validate (:value (get env (:value a)))) env a args expr)))

(defn validate-list [env {:keys [bracket-type elements] :as expr}]
  (case bracket-type
    :round (if (empty? elements)
             (invalid errors/empty-expression)
             (let
              [[a args] [(first elements) (rest elements)]
               a (validate-base true env a)]
               (if (:valid? a)
                 (validate-call env a args expr)
                 a)))
    :square (let
             [ls (map (partial validate env) elements)]
              (if (all-valid? ls)
                (assoc expr
                       :elements (vec ls)
                       :valid? true
                       :type (types/sequence-type (types/to-union (map :type ls)))
                       :environment env)
                (join-errors ls)))
    :curly errors/undefined))

(defn validate-base [accept-syntax? env a]
  (assert (= :syntax (:internal-type a)))
  (if (:valid? a)
    a
    (case (:syntax-type a)
      :literal (validate-literal env a)
      :symbol (validate-symbol accept-syntax? env a)
      :list (validate-list env a))))

(def validate (partial validate-base false))
