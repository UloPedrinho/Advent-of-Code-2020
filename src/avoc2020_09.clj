(ns avoc2020-09
  (:require [utils :as u]))

(def demo-input "day9-demo-input.txt")
(def input "day9-input.txt")

(defn format-transmision
  [file]
  (mapv #(Long/parseLong %) (line-seq (u/file-reader file))))

(defn is-valid-number?
  [number previous-numbers]
  (some (fn [n]
          (some #(and (not= n %)
                      (= number (+ n %))
                      true)
                previous-numbers)
          ) previous-numbers))

(defn get-invalid-number-position
  [transmision preamble-size]
  (let [trans    transmision
        position preamble-size]
    (loop [pos   position
           valid true]
      (if (nil? valid)
        (dec pos)
        (recur (inc pos) (is-valid-number?
                          (get trans pos)
                          (subvec trans (- pos preamble-size) pos)))))))

(let [transmision (format-transmision input)
      invalid-pos (get-invalid-number-position transmision 25)]
  (get transmision invalid-pos))

;; part2
(defn is-the-weakness?
  [transmision from to]
  (let [invalid-number (get transmision to)]
    (loop [pos from
           acc 0]
      (cond
        (= acc invalid-number) {:from from :to (dec pos)}
        (> acc invalid-number) nil
        (= pos (dec to))       nil
        :else
        (recur (inc pos) (+ acc (get transmision pos)))))))

(defn get-sum-weakness
  [transmision preamble invalid-number]
  (let [invalid-pos  (get-invalid-number-position transmision preamble)
        trans        (subvec transmision 0 (inc invalid-pos))
        weakness     (some #(if-let [weak (is-the-weakness?
                                           trans (.indexOf trans %) invalid-pos)]
                              weak)
                           trans)
        sorted-range (sort (subvec trans (:from weakness) (:to weakness)))]
    (println weakness)
    (+ (first sorted-range) (last sorted-range))))


(let [transmision    (format-transmision input)
      invalid-pos    (get-invalid-number-position transmision 25)
      invalid-number (get transmision invalid-pos)]
  (get-sum-weakness transmision 25 invalid-number))
