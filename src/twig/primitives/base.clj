(ns twig.primitives.base
  (:require [twig.print :as print]
            [clojure.string :as string]))

(defn default-print [indentation expr]
  (print/annotate (:type expr)
                  (:substitutions expr)
                  (str "\n" (print/pad (+ 3 indentation))
                       (print/bracket (:bracket-type expr)
                                      (string/join (str "\n" (print/pad (+ 4 indentation)))
                                                   (map (partial print/format-syntax (+ 4 indentation)) (:elements expr)))))))

(defn syntax [validate evaluate print]
  {:internal-type :primitive
   :validate validate
   :evaluate evaluate
   :print print})

(defn all-symbols? [ls]
  (reduce (fn [b a]
            (and b (= :symbol (:syntax-type a))))
          true
          ls))

(def default-syntax (symbol "apply"))
