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
;; (0 1 4 5 6 7 10 11 12 15 16 19 22)

(defn get-index-next
  [adapters pos increment]
  (let [jolts (+ (first adapters) increment)]
    (some #(if (= % jolts)
             (.indexOf adapters jolts))
          (rest adapters))))

(defn get-next
  [adapters pos increment]
  (let [last-pos (if (<= (+ pos 4) (count adapters))
                   (+ pos 4)
                   (count adapters))
        xs       (subvec adapters pos last-pos)
        i        (get-index-next xs pos increment)
        next     (and (not (nil? i)) (+ pos i))]
    next))

;; (0 1 4 5 6 7 10 11 12 15 16 19 22)
;; "Elapsed time: 2470.688723 msecs"
;; "Elapsed time: 998.695266 msecs"
;; "Elapsed time: 572.23647 msecs"

(defn no-name
  [adapters next res]
  (if (= next (dec (count adapters)))
    (conj res true)                     ;OPTIMIZE
    (for [x     [1 2 3]
          :let  [i (get-next adapters next x)]
          :when (not= i false)]
      (no-name adapters i res))))


(defn distinct-ways-of-arrange-adapters
  ""
  [input]
  (let [outlet-joltage  0
        lower-jolts     3
        builtin-adapter (+ (apply max input) lower-jolts)
        adapters        (vec (sort (into input [0 builtin-adapter])))]
    (count (filter true? (flatten (no-name adapters 0 '()))))))

(distinct-ways-of-arrange-adapters input)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (conj _ {:i (or (nil? i) (+ pos i)) :v (get adapters pos) :next next} )


;; (defn no-name
;;   [adapters pos]
;;   (reduce (fn [_ n]
;;             (let [last-pos (if (< = (+ pos 4) (count adapters))
;;                              (+ pos 4)
;;                              (count adapters))
;;                   xs       (subvec adapters pos last-pos)
;;                   i        (get-index-next xs pos n)
;;                   next     (and (not (nil? i)) (get adapters (+ pos i)))]
;;               (if (or (false? next) (nil? next))
;;                 _
;;                 (conj _ {:i (or (nil? i) (+ pos i)) :v (get adapters pos) :next next}))))
;;           []
;;           [1 2 3]))

;; (defn no-name-
;;   [adapters]
;;   ;; TODO: (conj #{} [1 2 3 ...]}
;;   (loop [pos 0]
;;     (cond
;;       (= pos (last adapters)) true
;;       (nil? pos)              false
;;       :else
;;       (recur ))
;;     ))


;; (defn distinct-ways-of-arrange-adapters
;;   ""
;;   [input]
;;   (let [outlet-joltage  0
;;         lower-jolts     3
;;         builtin-adapter (+ (apply max input) lower-jolts)
;;         adapters        (vec (sort (into input [0 builtin-adapter])))]
;;     adapters
;;     )
;;   )
