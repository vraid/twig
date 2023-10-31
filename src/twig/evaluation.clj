(ns twig.evaluation
  (:require [twig.values :as values]
            [twig.print :as print]
            [clojure.string :as string]))

(defn evaluate-literal [_ a]
  (:value a))

(defn evaluate-symbol [env a]
  (:value (get env (:value a))))

(declare evaluate)

(defn evaluate-call [env elements expr]
  (let
   [a (evaluate env (first elements))
    args (rest elements)]
    ((:evaluate a) env args expr)))

(defn evaluate-list [env {:keys [bracket-type elements] :as expr} type]
  (case bracket-type
    :round (evaluate-call env elements expr)
    :square (let
             [ls (map (partial evaluate env) elements)]
              (values/to-sequence type (vec ls)))
    :curly (assert false)))

(defn evaluate [env a]
  (assert (and (:valid? a)
               (= :syntax (:internal-type a)))
          (dissoc a :environment))
  (case (:syntax-type a)
    :literal (evaluate-literal env a)
    :symbol (evaluate-symbol env a)
    :list (evaluate-list env a (:type a))))

(defn entry [a]
  (println
   (if (:valid? a)
     (let
      [result (evaluate (:environment a) a)]
       (print/annotate (:type result) [] (:value result)))
     (string/join "\n" (:errors a))))
  [])
