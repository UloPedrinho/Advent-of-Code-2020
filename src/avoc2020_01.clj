(ns avoc2020-01
  (:require clojure.java.io))

;; part 1
(def demo-numbers [1721 979 366 299 675 1456])

(let [total 2020]
  (set (for [x     numbers
             y     numbers
             :when (= (+ x y) total)] (* x y)))) ;#{840324}

;; part 2
(def numbers (map #(Integer. %) (line-seq (clojure.java.aio/reader (clojure.java.io/resource "day1-input.txt")))))

(let [total 2020]
  (set (for [x     numbers
             y     numbers
             z     numbers
             :when (= (+ x y z) total)] (* x y z)))) ;#{170098110}
