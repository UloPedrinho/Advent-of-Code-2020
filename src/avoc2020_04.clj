(ns avoc2020-04
  (:require [clojure.string :as str]))

(def all-fields '(:byr :iyr :eyr :hgt :hcl :ecl :pid :cid))
(def optional-fields '(:cid))

(defn read-passports-file
  [file]
  (str/split (slurp (clojure.java.io/reader (clojure.java.io/resource file))) #"\n\n"))

(defn format-passports
  [unformated-passports]
  (map (fn [p]
         (let [u-passport (str/split p  #" |\n")]
           (reduce (fn [passport field]
                     (let [[k v] (str/split field #":")]
                       (conj passport {(keyword k) v})))
                   {}
                   u-passport)))
       unformated-passports))

(def demo-input (read-passports-file "day4-demo-input.txt"))

(defn valid-passports
  [passports]
  (reduce (fn [valid passport]
            (let [p-fields (count passport)
                  a-fields (count all-fields)
                  o-fields (count optional-fields)]
              (+ valid  (cond
                          (= p-fields a-fields)              1
                          (:cid passport)                    0
                          (< p-fields (- a-fields o-fields)) 0
                          :else                              1))))
          0
          passports))


(valid-passports (format-passports (read-passports-file "day4-demo-input.txt")))
(valid-passports (format-passports (read-passports-file "day4-input.txt")))

;; part 2
(defn in-range?
  [lmin lmax]
  #(and (>= (Integer/parseInt %) lmin) (<= (Integer/parseInt %) lmax) true))

(defn valid-regexp
  [regexp]
  #(re-find (read-string (str "#\"" regexp "\"")) %))

(def valid-byr? (in-range? 1920 2002))
(def valid-iyr? (in-range? 2010 2020))
(def valid-eyr? (in-range? 2020 2030))
(def valid-hgt-cm? (in-range? 150 193))
(def valid-hgt-in? (in-range? 59 76))
(defn valid-hgt?
  [hgt]
  (cond
    (and (str/ends-with? hgt "cm") (valid-hgt-cm?  (first (str/split hgt #"cm")))) true
    (and (str/ends-with? hgt "in") (valid-hgt-in?  (first (str/split hgt #"in")))) true
    :else                                                                          false))

(def valid-hcl? (valid-regexp "^#[0-9a-f]{6}$"))
(def valid-ecl? (valid-regexp "^amb|blu|brn|gry|grn|hzl|oth$"))
(def valid-pid? (valid-regexp "^[0-9]{9}$"))
(defn valid-cid? [cid] true)

(defn valid-passport?
  [passport]
  (not (some #(not ((resolve (symbol (str "valid-" (name %) "?"))) (% passport)))
        (keys passport))))

(defn valid-passports-all-data
  [passports]
  (reduce (fn [valid passport]
            (let [p-fields (count passport)
                  a-fields (count all-fields)
                  o-fields (count optional-fields)]
              (+ valid  (cond
                          (not (valid-passport? passport))   0
                          (= p-fields a-fields)              1
                          (:cid passport)                    0
                          (< p-fields (- a-fields o-fields)) 0
                          :else                              1))))
          0
          passports))
