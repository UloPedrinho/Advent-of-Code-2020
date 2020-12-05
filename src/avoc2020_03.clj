(ns avoc2020-03)

(def pattern-repeat 100)

;; part 1
(def demo-input  ["..##......."
                  "#...#...#.."
                  ".#....#..#."
                  "..#.#...#.#"
                  ".#...##..#."
                  "..#.##....."
                  ".#.#.#....#"
                  ".#........#"
                  "#.##...#..."
                  "#...##....#"
                  ".#..#...#.#"])

(def input (line-seq (clojure.java.io/reader (clojure.java.io/resource "day3-input.txt"))))

(defn make-line
  [pattern]
  (apply str (take pattern-repeat (repeat pattern))))

(defn make-map
  [input]
  (reduce (fn [map line]
            (conj map {:number (if (nil? (last map))
                                 0
                                 (inc (:number (last map))))
                       :line   (make-line line)}))
          [] input))

(defn count-trees
  [map right]
  (count (for [line  map
               :when (= (get (:line line) (* (:number line) right)) \#)]
           1)))

(defn count-trees
  [map slope]
  (count (for [line  map
               :when (= (get (:line line) (* (/ (:number line) (:down slope)) (:right slope))) \#)]
           1)))


(count-trees (make-map input) {:right 3 :down 1})

;; part 2
(apply * (let [map    (make-map input)
               slopes [{:right 1 :down 1}
                       {:right 3 :down 1}
                       {:right 5 :down 1}
                       {:right 7 :down 1}
                       {:right 1 :down 2}]]
           (reduce (fn [trees pattern ]
                     (conj trees (count-trees (take-nth (:down pattern) map)
                                              pattern)))
                   []
                   slopes)))
