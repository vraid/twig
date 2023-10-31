(ns twig.values
  (:require [twig.types :as types]
            [clojure.string :as string]))

(defn default-print [a]
  (:value a))

(defn print-specific [s]
  (fn [_] s))

(defn to-atom [type value]
  {:type type
   :value value
   :print default-print})

(defn to-sequence [type elements]
  {:type type
   :elements elements})

(defn to-function [type parameters evaluate]
  (let
   [s (str "(" (string/join " " (cons "function" parameters)) ")")]
    {:internal-type :function
     :type type
     :value s
     :evaluate evaluate
     :print (print-specific s)}))

(def false-value (symbol "false"))
(def true-value (symbol "true"))

(def false-atom (to-atom types/type-false false-value))
(def true-atom (to-atom types/type-true true-value))

(defn equals-false? [a] (= a false-atom))
