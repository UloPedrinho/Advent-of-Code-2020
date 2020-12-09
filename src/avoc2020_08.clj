(ns avoc2020-08
  (:require [utils :as u]
            [clojure.string :as s]))


(defn load-instructions
  [file]
  (line-seq (u/file-reader file)))

(defn format-instructions
  [instructions]
  (reduce (fn [v instruction]
            (let [splitted (s/split instruction #" ")]
              (conj v {:i (first splitted) :v (Integer/parseInt (last splitted))})))
          [] instructions))

(defn execute-instruction
  [instruction acc pos]
  (condp = (:i instruction)
    "acc" [(+ acc (:v instruction)) (inc pos)]
    "jmp" [acc (+ pos (:v instruction))]
    "nop" [acc (inc pos)]))


(defn run-code
  [instructions accumulator-init cycles log]
  (when log
    (println "cycl  ins val  | acc - pos")
    (println "---------------+----------"))

  (loop [accumulator accumulator-init
         position    0
         ins-counter 1
         ins-order   []]
    (cond (>= position (count instructions)) {:type "terminated" :acc accumulator}
          (> ins-counter cycles)             {:type "more-cycles" :acc accumulator}
          (some #(= % position) ins-order)   {:type "infinite-loop" :acc accumulator}
          :else
          (let [new-ins           (get instructions position)
                [new-acc new-pos] (execute-instruction new-ins
                                                       accumulator
                                                       position)]
            (when log
              (println (format "%04d: %s %4d | %2d - %3d" ins-counter (:i new-ins) (:v new-ins) new-acc position)))
            (recur new-acc new-pos (inc ins-counter) (conj ins-order position))))))

;; (run-code (format-instructions (load-instructions "day8-input.txt")) 0 1500 nil)

;; part 2

(defn switch-instruction
  [instruction]
  (if (= (:i instruction) "nop")
    {:i "jmp" :v (:v instruction)}
    {:i "nop" :v (:v instruction)}))

(defn patching-code
  [file]
  (let [instructions (format-instructions (load-instructions file))
        patch-index  (keep-indexed #(if (or (= (:i %2) "nop")
                                            (= (:i %2) "jmp"))
                                      %1) instructions)]
    (for [n     patch-index
          :let  [test-result (run-code (assoc instructions n (switch-instruction (get instructions n))) 0 1500 nil)]
          :when (= (:type test-result) "terminated")]
      test-result)))






