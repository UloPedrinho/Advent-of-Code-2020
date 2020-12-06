(ns utils)

(defn read-file-split-by-blackline
  [file]
  (clojure.string/split (slurp (clojure.java.io/reader (clojure.java.io/resource file))) #"\n\n"))
