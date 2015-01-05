(ns p-p-p-pokerface)

(defn rank [[r _]]
  (if (Character/isDigit r)
    (Integer/valueOf (str r))
    ({\T 10, \J 11, \Q 12, \K 13, \A 14} r)))

(defn suit [[_ s]]
  (str s))

(defn count-by [criteria hand]
  (->> hand (map criteria) (frequencies)))

(defn pair? [hand]
  (->> hand (count-by rank) (vals) (some #{2}) boolean))

(defn three-of-a-kind? [hand]
  (->> hand (count-by rank) (vals) (some #{3}) boolean))

(defn four-of-a-kind? [hand]
  (->> hand (count-by rank) (vals) (some #{4}) boolean))

(defn flush? [hand]
  (->> hand (count-by suit) (count) (== 1)))

(defn full-house? [hand]
  (->> hand (count-by rank) (vals) (every? #{2 3})))

(defn two-pairs? [hand]
  (let [freqs (->> hand (count-by rank) (vals))]
    (or (boolean (some #{4} freqs))
        (== 2 (count (filter #(== 2 %) freqs))))))

(defn straight? [hand]
  (let [s (set (map rank hand))
        range (- (apply max s) (apply min s))]
    (and (== 5 (count s))
         (or (== 4 range) (== 12 range)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (some (fn [[check score]] (when (check hand) score))
        [[straight-flush? 8] [four-of-a-kind? 7]  [full-house? 6] [flush? 5]
         [straight? 4]       [three-of-a-kind? 3] [two-pairs? 2]  [pair? 1] 
         [(fn [_] true) 0]]))
