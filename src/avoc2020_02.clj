(ns avoc2020-02)

;; part 1
(def demo-input ["1-3 a: abcde" "1-3 b: cdefg" "2-9 c: ccccccccc"])

(def input (line-seq (clojure.java.io/reader (clojure.java.io/resource "day2-input.txt"))))

(defn parse-passwords
  [input]
  (map (fn [line]
         (let [[limits [letter _] password] (clojure.string/split line #" ")
               [lmin lmax]                  (clojure.string/split limits #"-")]
           {:min (Integer. (str lmin)) :max (Integer. (str lmax)) :letter letter :password password}))
       input))

(let [passwords (parse-passwords input)]
  (count (filter (fn [{lmin     :min
                       lmax     :max
                       letter   :letter
                       password :password}]
                   (let [letter-times (count (re-seq (read-string (str "#\"" letter "\"")) password))]
                     (and (>= letter-times lmin)
                          (<= letter-times lmax)
                          true)))
                 passwords)))

;; part 2
(let [passwords (parse-passwords input)]
  (count (filter (fn [{lmin     :min
                       lmax     :max
                       letter   :letter
                       password :password}]
                   (let [lmin (dec lmin)
                         lmax (dec lmax)
                         pos1 (= (get password lmin) letter)
                         pos2 (= (get password lmax) letter)]
                     (and (not= pos1 pos2)
                          true)))
                 passwords)))
