(ns twig.unification
  (:require [twig.types :as types]))

(defn substitution [replacement value]
  {:internal-type :substitution :replacement replacement :value value})

(defn substitute [s a]
  (assert (= :type (:internal-type a))
          (:internal-type a))
  (let
   [subbed (reduce (fn [a b]
                     (assert (= :substitution (:internal-type b)))
                     (let
                      [sub (partial substitute [b])
                       replacement (:replacement b)
                       value (:value b)]
                       (case (:type a)
                         :primitive a
                         :variable (if (= a value) replacement a)
                         :union (types/to-union (map sub (:value a)))
                         :function (assoc a
                                          :parameters (map sub (:parameters a))
                                          :body (sub (:body a)))
                         :scheme (assoc a
                                        :value (sub (:value a))
                                        :variables (filter (fn [var] (not (= value var))) (:variables a)))
                         :sequence (assoc a :value (sub (:value a))))))
                   a
                   (reverse s))]
    (if (and (= :scheme (:type subbed))
             (empty? (:variables subbed)))
      (:value subbed)
      subbed)))

(defn remaining-variables [substitutions var]
  (filter types/variable? (map (partial substitute substitutions) var)))

(defn substitute-environment [s env]
  (into {} (map (fn [[k v]]
                  (let
                   [t (:type v)
                    t (if (= :syntax t) t (substitute s t))]
                    [k (assoc v :type t)]))
                env)))

(defn function-signature [a]
  (concat (:parameters a) [(:body a)]))

(defn occurs-in? [a b]
  (and
   (types/function? b)
   (reduce (fn [b t] (or b (= a t) (occurs-in? a t)))
           false
           (function-signature b))))

(defn unify-variable [a b]
  (let
   [[a b] (if (types/variable? a) [a b] [b a])]
    (and (not (and (types/function? b)
                   (occurs-in? a b)))
         [(substitution b a)])))

(declare most-general-unifier)

(defn unify-values-recursive [s a b]
  (assert (= (count a) (count b)))
  (if (empty? a)
    s
    (let
     [sf (comp (partial substitute s) first)
      mgu (most-general-unifier (sf a) (sf b))]
      (if mgu
        (recur (concat mgu s) (rest a) (rest b))
        false))))

(defn unify-function [a b]
  (and (= (count (:parameters a))
          (count (:parameters b)))
       (unify-values-recursive [] (function-signature a) (function-signature b))))

(defn unify-sequence [a b]
  (most-general-unifier (:value a) (:value b)))

(defn most-general-unifier [a b]
  (let
   [a (types/descheme a)
    b (types/descheme b)]
    (cond
      (= a b) []
      (or (types/variable? a)
          (types/variable? b)) (unify-variable a b)
      (and (types/function? a)
           (types/function? b)) (unify-function a b)
      (and (types/sequence? a)
           (types/sequence? b)) (unify-sequence a b)
      :else false)))
