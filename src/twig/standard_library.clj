(ns twig.standard-library
  (:require [twig.types :as types]
            [twig.values :as values]
            [twig.primitives.base :as primitives]))

(defn proc [type evaluate]
  {:internal-type :function
   :type type
   :evaluate evaluate
   :print primitives/default-print})

(def add-proc
  (proc
   (types/function-type [types/type-number types/type-number] types/type-number)
   (fn [args]
     (let
      [[a b] args]
       (values/to-atom types/type-number (+ (:value a) (:value b)))))))

(def negative
  (proc
   (types/function-type [types/type-number] types/type-number)
   (fn [args]
     (let
      [[a] args]
       (values/to-atom types/type-number (- (:value a)))))))
