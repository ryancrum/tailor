(ns tailor.test.diff
  (:use [tailor.diff])
  (:use [clojure.test]))

(deftest generate-hunk-header
  (is (= "@@ -3,5 +6,7 @@" (#'tailor.diff/generate-hunk-header 3 5 6 7))))

(deftest clamp-window-to-range
  (is (= [0 3] (#'tailor.diff/clamp-window-to-range [3 5] 4 3))))

(deftest lazy-subseq
  (is (= '(2 3 4) (#'tailor.diff/lazy-subseq '(1 2 3 4 5) [1 3]))))

(deftest cluster-seq
  (is (= [[1 2 3] [5 6 7]] (#'tailor.diff/cluster-seq [1 2 3 5 6 7] 1)))
  (is (= [[1 2] [4] [6 7 8]] (#'tailor.diff/cluster-seq [1 2 4 6 7 8] 1))))

(deftest change-count
  (is (= 3 (#'tailor.diff/change-count
            {:change-map {0 :add 2 :add 3 :add 4 :remove}}
            :add)))
  (is (= 1 (#'tailor.diff/change-count
            {:change-map {0 :add 2 :add 3 :add 4 :remove}}
            :remove))))

(deftest net-change-count
  (is (= 0 (#'tailor.diff/net-change-count
            {:change-map {0 :add 1 :remove 2 :add 3 :remove}})))
  (is (= 1 (#'tailor.diff/net-change-count
            {:change-map {0 :add 1 :remove 2 :add}})))
  (is (= -1 (#'tailor.diff/net-change-count
             {:change-map {0 :add 1 :remove 2 :remove}}))))

(deftest line-index
  (let [changeset (atom (create-changeset ["a" "b" "c"]))]
    (is (= 2 (#'tailor.diff/line-index @changeset 3)))
    (is (= 0 (#'tailor.diff/line-index @changeset 1)))
    (is (= 2 (#'tailor.diff/line-index @changeset 5)))
    (swap! changeset append-line "z" 4)
    (is (= 2 (#'tailor.diff/line-index @changeset 3)))
    (is (= 3 (#'tailor.diff/line-index @changeset 5)))
    (swap! changeset remove-line 3)
    (is (= 3 (#'tailor.diff/line-index @changeset 3)))
    (is (= 1 (#'tailor.diff/line-index @changeset 2)))
    (swap! changeset insert-line "A" 1)
    (is (= 0 (#'tailor.diff/line-index @changeset 1)))
    (is (= 1 (#'tailor.diff/line-index @changeset 2)))))

