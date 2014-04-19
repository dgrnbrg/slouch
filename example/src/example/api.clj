(ns example.api)

(defn  ^:export product [& nums]
  {:pre [every? number? nums]}
  (apply * nums))

(defn ^:export mean [nums]
  {:pre [every? number? nums]}
  (/ (apply + nums) (count nums)))

(defn ^:export median [nums]
  {:pre [every? number? nums]}
  (let [c (count nums)
        sorted (vec (sort nums))]
    (if (odd? c)
      (get sorted (/ (dec c) 2))
      (mean (let [mid (/ c 2)]
              (subvec sorted (dec mid) (inc mid)))))))

(defn ^:export mode [nums]
  {:pre [every? number? nums]}
  (->> nums
       frequencies
       (sort-by val)
       last
       key))

(defn ^:export stats-meta [& nums]
  {:pre [every? number? nums]}
  (with-meta
    nums
    {:mean (mean nums)
     :median (median nums)
     :mode (mode nums)}))
