(ns avoc2020-11
  (:require [utils :as u]))

(def demo-input "day11-demo-input.txt")
(def input "day11-input.txt")

(defn file-to-vec-grid
  [file]
  (reduce #(conj %1 (vec %2))
          []
          (line-seq (u/file-reader file))))


(def around-cells '([-1 -1] [-1 0]
                    [-1 1] [0 -1]
                    [0 1] [1 -1]
                    [1 0]  [1 1]))

(defn around-positions
  [pos]
  (reduce #(conj %1 [(+ (first pos) (first %2))
                     (+ (last pos) (last %2))])
          []
          around-cells))

(defn seats-around
  [grid pos]
  (reduce (fn [m p]
            (if-let [cell (get-in grid p)]
              (update m cell inc)
              m))
          {\L 0 \# 0 \. 0}
          (around-positions pos)))

(defn one-cycle
  [grid]
  (reduce (fn [g p]
            (let [current (get-in grid p)
                  around  (seats-around grid p)]
              (cond
                (and (= current \L) (= (get around \#) 0))
                (assoc-in g p \#)
                #_                                          (and (= current \#) (>= (get around \#) 4)) ; part1
                (and (= current \#) (>= (get around \#) 5)) ; part2
                (assoc-in g p \L)
                :else                                       g)))
          grid
          (for [x (range 0 (count grid))
                y (range 0 (count (first grid)))]
            [x y])))

(defn find-seat-cycle
  [grid]
  (loop [current-grid grid
         old-grid     []
         cycles       0]
    (if (= current-grid old-grid)
      current-grid
      (recur (one-cycle current-grid) current-grid (inc cycles)))))

(defn ocupated-sites
  [grid]
  (count (filter #{\#} (flatten (find-seat-cycle grid)))))

(ocupated-sites (file-to-vec-grid input))

;; part2
(defn add-increment-to-pos
  [pos increment]
  [(+ (first pos) (first increment))
   (+ (last pos) (last increment))])

(defn seats-around
  [grid pos]
  (reduce (fn [m increment]
            (let [current (add-increment-to-pos pos increment)]
              (loop [p    current
                     cell (get-in grid p)]
                (if (not= cell \.)
                  (if (nil? cell)
                    m
                    (update m cell inc))
                  (recur (add-increment-to-pos p increment)
                         (get-in grid (add-increment-to-pos p increment)))))
              )
            )
          {\L 0 \# 0 \. 0}
          around-cells))

(ocupated-sites (file-to-vec-grid input))

