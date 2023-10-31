(ns twig.primitives.let
  (:require [twig.types :as types]
            [twig.syntax :as syntax]
            [twig.primitives.base :as primitives]
            [twig.environments :as environments]
            [twig.unification :as unification]
            [twig.validation :as validation]
            [twig.evaluation :as evaluation]
            [twig.print :as print]
            [clojure.string :as string]))

(defn valid-bindings? [ls]
  (and (even? (count ls))
       (let
        [names (map (fn [n] (nth ls (* 2 n)))
                    (range (/ (count ls) 2)))]
         (primitives/all-symbols? names))))

(defn validate-let-bindings [env a args body expr]
  (let
   [f (fn [validated env substitutions bindings]
        (if (empty? bindings)
          (let
           [body (validation/validate env body)]
            (if (not (:valid? body))
              body
              (let
               [substitutions (concat (:substitutions body) substitutions)]
                (assoc expr
                       :valid? true
                       :elements [(assoc a :type (types/primitive-type :syntax)) (assoc args :elements validated) body]
                       :type (:type body)
                       :variables (apply concat (map :variables (cons body (map second validated))))
                       :substitutions substitutions
                       :environment env))))
          (let
           [[name value] (take 2 bindings)
            value (validation/validate env value)]
            (if (not (:valid? value))
              value
              (recur (concat validated [name value])
                     (assoc (unification/substitute-environment substitutions env)
                            (:value name)
                            (environments/bind value))
                     (concat (:substitutions value) substitutions)
                     (drop 2 bindings))))))]
    (f [] env [] (:elements args))))

(defn pairwise-bindings [bindings]
  (map (fn [n]
         [(nth bindings (* 2 n))
          (nth bindings (+ 1 (* 2 n)))])
       (range (/ (count bindings) 2))))

(defn print-let [indentation expr]
  (let
   [[a bindings body] (:elements expr)
    bindings (pairwise-bindings (:elements bindings))]
    (print/annotate (:type expr)
                    (:substitutions expr)
                    (str "\n" (print/pad (+ 3 indentation))
                         (print/bracket (:bracket-type expr)
                                        (string/join (str "\n" (print/pad (+ 4 indentation)))
                                                     (concat [(print/format-syntax 0 a)]
                                                             [(print/bracket :round
                                                                             (string/join (str "\n" (print/pad (+ 5 indentation)))
                                                                                          (map (fn [[k v]]
                                                                                                 (str (:value k) " " (print/format-syntax (+ 5 indentation) v)))
                                                                                               bindings)))]
                                                             [(print/format-syntax (+ 4 indentation) body)])))))))

(defn validate-let [env a args expr]
  (if (not (= 2 (count args)))
    (validation/invalid ["malformed let statement"])
    (let
     [[bindings body] args
      binding-list (:elements bindings)]
      (if (not (valid-bindings? binding-list))
        (validation/invalid [(str "invalid let binding: " binding-list)])
        (validate-let-bindings env a bindings body expr)))))

(defn eval-let [env args _]
  (let
   [[bindings body] args
    f (fn [env bindings]
        (if (empty? bindings)
          (evaluation/evaluate env body)
          (let
           [[name value] (take 2 bindings)]
            (recur (assoc env (:value name) (environments/bind (evaluation/evaluate env value)))
                   (drop 2 bindings)))))]
    (f env (:elements bindings))))

(def let-syntax
  (primitives/syntax
   validate-let
   eval-let
   print-let))

(defn validate-letrec-bindings [env a args body expr]
  (let
   [extend-env (fn [env bindings]
                 (if (empty? bindings)
                   env
                   (let
                    [[name _] (take 2 bindings)]
                     (recur (assoc env
                                   (:value name)
                                   (environments/bind (syntax/type-placeholder (types/variable-type))))
                            (drop 2 bindings)))))
    bindings (:elements args)
    env (extend-env env bindings)
    f (fn [validated env substitutions bindings]
        (if (empty? bindings)
          [true validated substitutions env]
          (let
           [[name value] (take 2 bindings)
            value (validation/validate env value)]
            (if (not (:valid? value))
              [false value]
              (let
               [var (:type (get env (:value name)))
                substitutions (concat (if (types/variable? var)
                                        [(unification/substitution (:type value) var)] [])
                                      (concat (:substitutions value) substitutions))]
                (recur (concat validated [name value])
                       (assoc (unification/substitute-environment substitutions env)
                              (:value name)
                              (environments/bind value))
                       substitutions
                       (drop 2 bindings)))))))
    [valid? validated initial _] (f [] env [] bindings)]
    (if (not valid?)
      validated
      (let
       [[valid? validated substitutions env] (f [] (unification/substitute-environment initial env) initial bindings)]
        (if (not valid?)
          validated
          (let
           [body (validation/validate env body)]
            (if (not (:valid? body))
              body
              (let
               [substitutions (concat (:substitutions body) substitutions)]
                (assoc expr
                       :valid? true
                       :elements [(assoc a :type (types/primitive-type :syntax)) (assoc args :elements validated) body]
                       :type (:type body)
                       :variables (apply concat (map :variables (cons body (map second validated))))
                       :substitutions substitutions
                       :environment env)))))))))

(defn validate-letrec [env a args expr]
  (if (not (= 2 (count args)))
    (validation/invalid ["malformed letrec statement"])
    (let
     [[bindings body] args
      binding-list (:elements bindings)]
      (if (not (valid-bindings? binding-list))
        (validation/invalid [(str "invalid letrec binding: " binding-list)])
        (validate-letrec-bindings env a bindings body expr)))))

(defn eval-letrec [env args _]
  (let
   [[bindings body] args
    f (fn [env bindings]
        (if (empty? bindings)
          (evaluation/evaluate env body)
          (let
           [[name value] (take 2 bindings)]
            (recur (assoc env (:value name) (environments/bind (evaluation/evaluate env value)))
                   (drop 2 bindings)))))]
    (f env (:elements bindings))))

(def letrec-syntax
  (primitives/syntax
   validate-letrec
   eval-letrec
   print-let))
