(ns twig.primitives.lambda
  (:require [twig.types :as types]
            [twig.values :as values]
            [twig.primitives.base :as primitives]
            [twig.environments :as environments]
            [twig.unification :as unification]
            [twig.validation :as validation]
            [twig.evaluation :as evaluation]
            [twig.print :as print]
            [clojure.string :as string]))

(defn valid-params? [ls]
  (and
   (primitives/all-symbols? ls)
   (= (count ls) (count (into #{} (map :value ls))))))

(defn to-param [a]
  [a (environments/parameter a)])

(defn validate-lambda [env a args expr]
  (if (not (= 2 (count args)))
    (validation/invalid ["malformed lambda statement"])
    (let
     [[params body] args
      param-list (:elements params)]
      (if (not (valid-params? param-list))
        (validation/invalid [(str "invalid param list: " param-list)])
        (let
         [params (map (comp to-param :value) param-list)
          body (validation/validate (into env params) body)]
          (if (:valid? body)
            (let
             [substitutions (:substitutions body)
              var (mapv (comp :type second) params)
              scheme (unification/substitute substitutions (types/scheme-type (concat var (:variables body))
                                                                              (types/function-type var (:type body))))]
              (assoc (validation/with-type scheme expr)
                     :valid? true
                     :elements [(assoc a :type (types/primitive-type :syntax)) (first args) body]
                     :substitutions substitutions
                     :environment env
                     :print primitives/default-print))
            body))))))

(defn eval-lambda [env args expr]
  (let
   [[params body] args
    params (:elements params)]
    (values/to-function
     (:type expr)
     (map :value params)
     (fn [args]
       (let
        [bindings (map (fn [n] [(:value (nth params n))
                                (environments/bind (nth args n))])
                       (range (count params)))
         env (into env bindings)]
         (evaluation/evaluate env body))))))

(defn print-lambda [indentation expr]
  (let
   [[a parameters body] (:elements expr)]
    (print/annotate (:type expr)
                    (:substitutions expr)
                    (str "\n" (print/pad (+ 3 indentation))
                         (print/bracket (:bracket-type expr)
                                        (string/join (str "\n" (print/pad (+ 4 indentation)))
                                                     (concat [(print/format-syntax 0 a)]
                                                             [(print/bracket :round (string/join " " (map :value (:elements parameters))))]
                                                             [(print/format-syntax (+ 4 indentation) body)])))))))

(def lambda-syntax
  (primitives/syntax
   validate-lambda
   eval-lambda
   print-lambda))
