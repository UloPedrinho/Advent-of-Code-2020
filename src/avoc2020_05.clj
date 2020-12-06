(ns avoc2020-05)

(def row-min 0)
(def row-max 127)
(def col-min 0)
(def col-max 7)

(def input (line-seq (clojure.java.io/reader (clojure.java.io/resource "day5-input.txt"))))

(defn get-binary-partition
  [lower upper lmin lmax]
  #(:min (reduce (fn [col letter]
                 (let [half (/ (- (:max col) (:min col)) 2)]
                                  (cond
                                    (= letter lower) {:min (:min col) :max (+ (:min col) (int (Math/floor half)))}
                                    (= letter upper) {:min (+ (:min col) (int (Math/ceil half))) :max (:max col)})))
                 {:min lmin :max lmax}
                 %)))

(def get-row (get-binary-partition \F \B row-min row-max))
(def get-col (get-binary-partition \L \R col-min col-max))

(defn get-seats
  [pattern]
  (map #(let [row-chars (subs % 0 7)
              col-chars (subs % 7 10)]
          (+ (* (get-row row-chars) (inc col-max)) (get-col col-chars)))
       pattern))

(defn get-highest-seat
  [seats]
  (apply max (get-seats seats)))

(get-highest-seat input)

;; part 2
(defn my-seat
  [seats]
  (let [exist   (sort (get-seats seats))
        v-seats (reduce #(assoc %1 %2 1)
                        (into [] (repeat (inc (last exist)) nil))
                        exist)]
    (first (for [seat  (range 1 (last exist))
                 :when (and (= (get v-seats seat) nil)
                            (= (get v-seats (dec seat)) 1)
                            (= (get v-seats (inc seat)) 1))]
             seat))))

