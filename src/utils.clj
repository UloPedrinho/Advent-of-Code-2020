(ns utils)

(defn file-reader
  [path]
  (clojure.java.io/reader (clojure.java.io/resource path)))

(defn read-file-split-by-blackline
  [file]
  (clojure.string/split (slurp (file-reader file)) #"\n\n"))

(defn format-str-into-int
  [file]
  (mapv #(Long/parseLong %) (line-seq (file-reader file))))
