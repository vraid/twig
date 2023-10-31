(ns twig.read
  (:require [twig.syntax :as syntax]
            [twig.values :as values]
            [twig.types :as types]))

(defn read-next [ls]
  [(first ls) (if (= 1 (count ls)) \newline (second ls)) (rest ls)])

(def whitespace [\space \newline])
(def opening-brackets [\( \[ \{])
(def closing-brackets [\) \] \}])
(def matching-bracket {\( \) \[ \] \{ \}})
(def bracket-type {\) :round \] :square \} :curly})

(defn parse-string [n res ls]
  (assert (seq ls))
  (let
   [[a b ls] (read-next ls)]
    (cond
      (= \" a) (do
                 (assert (some #{b} (concat closing-brackets whitespace)))
                 [(apply str (reverse res)) (+ 2 n) ls])
      :else (recur (inc n) (cons a res) ls))))

(defn parse-number [[a n ls]]
  (try
    [(Integer/parseInt a) n ls]
    (catch Exception _
      (try
        [(Float/parseFloat a) n ls]
        (catch Exception _ (str "couldn't parse number " a))))))

(defn parse-symbol [[a n ls]]
  [a n ls])

(defn read-atom [disallowed n res ls]
  (if (empty? ls)
    res
    (let
     [[a b ls] (read-next ls)]
      (assert (not (some #{b} disallowed)))
      (if (some #{b} (concat closing-brackets whitespace))
        [(apply str (reverse (cons a res))) (inc n) ls]
        (recur disallowed (inc n) (cons a res) ls)))))

(defn parse-atom [str start a ls]
  (let
   [disallowed [\( \"]]
    (cond
      (= \" a) (let
                [[a end ls] (parse-string start [] (if (empty? ls) [] (rest ls)))]
                 [(syntax/syntax-literal (syntax/origin str start end) (values/to-atom types/type-string a)) end ls])
      (some #{a} [\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \.]) (let
                                                      [[a end ls] (parse-number (read-atom disallowed start [] ls))]
                                                       [(syntax/syntax-literal (syntax/origin str start end) (values/to-atom types/type-number a)) end ls])
      :else (let
             [[a end ls] (parse-symbol (read-atom disallowed start [] ls))]
              [(syntax/syntax-symbol (syntax/origin str start end) a)
               end
               ls]))))

(defn parse-literal [n ls]
  (read-atom [\"] n [] ls))

(defn eval-boolean [value]
  (fn [origin _] (syntax/syntax-literal origin value)))

(def eval-false (eval-boolean values/false-atom))
(def eval-true (eval-boolean values/true-atom))

(def literals
  {"f" eval-false
   "t" eval-true})

(defn eval-literal [origin a args]
  (assert (contains? literals a) a)
  ((literals a) origin args))

(defn parse-tokens [str start brackets res list]
  (if (empty? list)
    (do
      (assert (empty? brackets))
      (reverse res))
    (let
     [[a b ls] (read-next list)]
      (cond
        (some #{a} whitespace) (recur str (inc start) brackets res ls)
        (some #{a} opening-brackets) (let
                                      [[a end ls] (parse-tokens str (inc start) (cons (syntax/bracket start a) brackets) [] ls)]
                                       (recur str end brackets (cons a res) ls))
        (some #{a} closing-brackets) (do
                                       (assert (seq brackets))
                                       (assert (= a (matching-bracket (:type (first brackets)))))
                                       (assert (some #{b} (concat closing-brackets whitespace)))
                                       [(syntax/syntax-list (syntax/origin str (:location (first brackets)) (inc start)) (bracket-type a) (reverse res))
                                        (inc start)
                                        ls])
        (= \# a) (let
                  [[literal end ls] (parse-literal (inc start) ls)
                   [a _ _] (read-next ls)
                   [args end ls] (if (some #{a} opening-brackets)
                                   (parse-tokens str (inc start) (cons (syntax/bracket end a) brackets) [] ls)
                                   [false end ls])]
                   (recur str end brackets (cons (eval-literal (syntax/origin str start end) literal args) res) ls))
        :else (let
               [[a end ls] (parse-atom str start a list)]
                (recur str end brackets (cons a res) ls))))))

(defn parse [str]
  (parse-tokens str 0 [] [] str))
