(ns twig.errors)

(def undefined ["undefined"])
(def empty-expression ["empty expression"])
(defn undefined-variable [value] [(str "undefined variable: " value)])
(defn invalid-application [value] [(str "application of: " value)])
(defn type-mismatch [_ _] [(str "type mismatch")])
