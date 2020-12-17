(ns avoc2020-10
  (:require [utils :as u]))

(def demo1-adapters [16 10 15 5 1 11 7 19 6 12 4])
(def demo2-adapters [28 33 18 42 31 14 46 20 48 47
                     24 23 49 45 19 38 39 11 1 32
                     25 35 8 17 7 9 4 2 34 10 3 ])

(def input (u/format-str-into-int "day10-input.txt"))

(defn get-jolt-difference
  [input]
  (let [lower-jolts     3
        outlet-joltage  0
        builtin-adapter (+ (apply max input) lower-jolts)
        adapters        (sort (conj input builtin-adapter))
        inc-diff        (fn [difference res n] (if (= difference n)
                                                 (inc (get res n))
                                                 (get res n)))]

    (reduce (fn [res jolt]
              (let [difference (- jolt (:total res))
                    total      (+ (:total res) difference)
                    dif-1      (inc-diff difference res 1)
                    dif-3      (inc-diff difference res 3)]
                {1 dif-1 3 dif-3 :total total :pos (inc (:pos res))}
                ))
            {1 0 3 0 :total 0 "pos" 0}
            adapters)))

;; part2
(defn get-val
  [ma pos]
  (let [v (get ma pos)]
    (if (nil? v)
      0
      v)))

(defn number-of-arrange-adapters
  ""
  [input]
  (let [adapters (vec (sort input))]
    (last (last (sort  (reduce (fn [m jolt]
                                 (let [j1    (get-val m (- jolt 1))
                                       j2    (get-val m (- jolt 2))
                                       j3    (get-val m (- jolt 3))
                                       value (+ j1 j2 j3)]
                                   (assoc m jolt value)))
                               {0 1}
                               adapters))))))

(number-of-arrange-adapters input)
