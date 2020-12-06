(ns avoc2020-06
  (:require [clojure.string :as str]
            [utils :as u]))

(def input (u/read-file-split-by-blackline "day6-input.txt"))

(defn format-input-to-groups
  [input]
  (map #(str/split % #"\n") input))

(defn get-group-answers
  ""
  [group]
  (reduce #(into %1 %2) #{} group))

(defn get-groups-answers
  [groups]
  (map #(get-group-answers %) groups))

(apply + (map count (get-groups-answers (format-input-to-groups input))))

;; part 2
(defn get-group-common-answers-vector
  [group]
  (let [v-answers (into [] (repeat (inc (- (int \z) (int \a))) 0))]
    (reduce #(reduce (fn [v a]
                       (let [pos (- (int \z) (int a))]
                         (assoc v pos (inc (get v pos)))))
                     %1 %2)
            v-answers
            group)))

(defn get-group-common-answers-count
  [group]
  (let [componets (count group)
        v-answers (get-group-common-answers-vector group)]
    (reduce #(if (= componets %2) (inc %1) %1) 0 v-answers)))


(defn get-groups-common-answers-count
  [groups]
  (reduce #(+ %1 (get-group-common-answers-count %2)) 0 groups))

(get-groups-common-answers-count (format-input-to-groups input))
