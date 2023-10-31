(ns twig.print
  (:require [clojure.string :as string]))

(defn bracket [t a]
  (case t
    :round (str "(" a ")")
    :square (str "[" a "]")
    :curly (str "{" a "}")))

(defn format-type [a]
  (case (:type a)
    :primitive (name (:value a))
    :union (bracket :round (str (string/join " " (cons "either" (map format-type (:value a))))))
    :variable (:value a)
    :scheme (bracket :round (str "scheme " (bracket :round (string/join " " (map :value (:variables a)))) " " (format-type (:value a))))
    :function (bracket :round (str "-> " (string/join " " (map format-type (concat (:parameters a) [(:body a)])))))
    :sequence (bracket :round (str "list " (format-type (:value a))))))

(defn format-substitution [a]
  (assert (= :substitution (:internal-type a)))
  (bracket :round (str "\\ " (format-type (:value a)) " " (format-type (:replacement a)))))

(defn annotate [type _ value]
  (if type
    (str "(: " (format-type type) " " value ")")
    value))

(defn pad [n]
  (apply str (take n (repeat " "))))

(defn literal-value [a]
  ((:print a) a))

(declare format-syntax)

(defn format-syntax [indentation expr]
  (case (:syntax-type expr)
    :literal (annotate (:type expr) [] (literal-value (:value expr)))
    :symbol (annotate (:type expr) (:substitutions expr) (:value expr))
    :list (case (:bracket-type expr)
            :round (let
                    [a (first (:elements expr))
                     a (get (:environment expr) (:value a))]
                     ((:print (:value a)) indentation expr))
            :square (annotate (:type expr)
                              (:substitutions expr)
                              (str "\n" (pad (+ 3 indentation))
                                   (bracket :square (string/join (str "\n" (pad (+ 4 indentation)))
                                                                 (map (partial format-syntax (+ 4 indentation)) (:elements expr))))))
            :curly "")))

(defn print-syntax [a]
  (println (if (:valid? a)
             (str (format-syntax 0 a) "\n")
             (string/join "\n" (:errors a))))
  [])

(defn print-origin [a]
  (println (subs (:string a) (:start a) (:end a))))

(defn print-expression [a]
  (case (:syntax-type a)
    :list (do (print-origin (:origin a))
              (for [a (:elements a)]
                (print-expression a)))
    (print-origin (:origin a))))
