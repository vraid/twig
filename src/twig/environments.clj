(ns twig.environments
  (:require [twig.types :as types]))

(defn literal [a]
  {:internal-type :literal
   :type (:type a)
   :value a})

(defn bind [a]
  {:internal-type :binding
   :type (:type a)
   :value a})

(defn parameter [a]
  {:internal-type :parameter
   :type (types/variable-type)
   :name a
   :value {:print false}})

(defn special [a]
  {:internal-type :special
   :type :syntax
   :value a})
