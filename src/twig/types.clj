(ns twig.types)

(defn primitive-type [value]
  {:internal-type :type
   :type :primitive
   :value value})

(defn variable-type []
  {:internal-type :type
   :type :variable
   :value (gensym)})

(defn function-type [parameters body]
  {:internal-type :type
   :type :function
   :parameters parameters
   :body body})

(defn scheme-type [variables value]
  {:internal-type :type
   :type :scheme
   :variables (into #{} variables)
   :value value})

(defn union-type [value]
  {:internal-type :type
   :type :union
   :value value})

(defn descheme [a]
  (if (= :scheme (:type a))
    (recur (:value a))
    a))

(defn scheme-variables [a]
  (let
   [a (descheme a)]
    (if (= :scheme (:type a))
      (vec (:variables a))
      [])))

(def empty-union (union-type #{}))

(defn union-value [a]
  (if (= :union (:type a))
    (:value a)
    [a]))

(defn extend-union [union types]
  (assert (= :union (:type union)))
  (let
   [a (union-type (into (:value union) (apply concat (map union-value types))))
    unique (:value a)]
    (if (= 1 (count unique))
      (first unique)
      a)))

(defn to-union [types]
  (extend-union empty-union types))

(defn sequence-type [value]
  {:internal-type :type
   :type :sequence
   :value value})

(defn sequence-value [a]
  (:value a))

(defn assert-type [a]
  (assert (= :type (:internal-type a))))

(defn is-type [t]
  (fn [a]
    (assert-type a)
    (= t (:type a))))

(def variable? (is-type :variable))
(def function? (is-type :function))
(def scheme? (is-type :scheme))
(def sequence? (is-type :sequence))

(def type-symbol (primitive-type :symbol))
(def type-string (primitive-type :string))
(def type-false (primitive-type :false))
(def type-true (primitive-type :true))
(def type-boolean (to-union [type-false type-true]))
(def type-number (primitive-type :number))
