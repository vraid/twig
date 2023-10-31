(ns twig.syntax)

(defn origin [str start end]
  {:string str
   :start start
   :end end})

(defn origin-str [origin]
  (subs (:string origin)
        (:start origin)
        (:end origin)))

(def no-origin (origin "" 0 0))

(defn bracket [location type]
  {:location location
   :type type})

(defn syntax-literal [origin value]
  {:internal-type :syntax
   :syntax-type :literal
   :origin origin
   :type (:type value)
   :variables []
   :substitutions []
   :environment {}
   :value value})

(defn syntax-symbol [origin a]
  {:internal-type :syntax
   :syntax-type :symbol
   :origin origin
   :type false
   :variables []
   :substitutions []
   :environment false
   :value a})

(defn syntax-list [origin bracket-type elements]
  {:internal-type :syntax
   :syntax-type :list
   :origin origin
   :bracket-type bracket-type
   :type false
   :variables []
   :substitutions []
   :environment false
   :elements elements})

(defn type-placeholder [type]
  {:internal-type :syntax
   :syntax-type :placeholder
   :type type
   :value "placeholder"})
