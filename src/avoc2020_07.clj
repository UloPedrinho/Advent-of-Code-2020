(ns avoc2020-07
  (:require [utils :as u]
            [clojure.string :as str]))

(defn parse-inner
  [bags]
  (reduce (fn [inner bag]
            (let [clean    (str/replace bag #"bags.|bag." "")
                  splitted (str/split (str/trim clean) #" " 2)]
              (if (= (first splitted) "no")
                nil
                (conj inner [ (last splitted) (Integer/parseInt (first splitted))]))))
          {} (str/split bags #"bags,|bag,")))


(defn parse-rule
  [raw-rule]
  (let [[container inner] (str/split raw-rule #"bags contain")]
    {(keyword (str/trim container)) (parse-inner inner)}))


(defn parse-rules
  [file]
  (let [raw-rules-list (line-seq (u/file-reader file))]
    (reduce (fn [r i]
              (conj r (parse-rule i)))
            {} raw-rules-list)))

(defn bag-inside?
  [bag inside]
  (some #(= bag %) (keys (val inside))))

(defn which-contains-this-bag
  [bag rules]
  (reduce (fn [which rule]
            (if (bag-inside? bag rule)
              (conj which (key rule))
              which))
          [] rules))

(defn in-which-bags
  ([bags rules which]
   (if (empty? bags)
     which
     (let [new-bags (which-contains-this-bag (name (first bags)) rules)]
       (recur  (into (rest bags) new-bags)
               rules
               (set (into which new-bags)))))))

(defn how-many-bags-can-contain
  [bag rules]
  (count (let [which (which-contains-this-bag bag rules)]
           (in-which-bags which rules which))))


(how-many-bags-can-contain "shiny gold" (parse-rules "day7-input.txt"))

;; part 2
(defn how-many-bags-contain
  [bags rules how-many]
  (reduce (fn [total bag]
            (let [current ((keyword (first bag)) rules)]
              (if (empty? current)
                (+ how-many (second bag))
                (+ how-many (* (second bag) (how-many-bags-contain current rules how-many)))
                )))
          how-many
          bags))

(defn how-many-bags-contain
  [bags rules how-many]
  (if (empty? bags)
    how-many
    (recur (rest bags)
           rules
           (+ how-many (* (second (first bags))
                          (how-many-bags-contain ((keyword (first (first bags))) rules)
                                                 rules 1))))))

(defn how-many-must-contain
  [bag rules]
  (let [which ((keyword bag) rules ) ]
    (how-many-bags-contain which rules 0)))
