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

(deftest test-line-index
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

(deftest test-index-line
  (let [changeset (atom (create-changeset ["a" "b" "c"]))]
    (is (= 3 (#'tailor.diff/index-line @changeset 2)))
    (is (= 2 (#'tailor.diff/index-line @changeset 1)))
    (is (= 1 (#'tailor.diff/index-line @changeset 0)))
    (is (= 3 (#'tailor.diff/index-line @changeset 4)))
    (swap! changeset append-line "z" 4)
    (is (= 4 (#'tailor.diff/index-line @changeset 3)))
    (is (= 3 (#'tailor.diff/index-line @changeset 2)))
    (swap! changeset remove-line 3)
    (is (= 3 (#'tailor.diff/index-line @changeset 3)))
    (is (= 3 (#'tailor.diff/index-line @changeset 2)))
    (is (= 2 (#'tailor.diff/index-line @changeset 1)))
    (swap! changeset insert-line "A" 1)
    (is (= 1 (#'tailor.diff/index-line @changeset 0)))
    (is (= 3 (#'tailor.diff/index-line @changeset 2)))))

(deftest shift-change-map
  (is (= {2 :add 4 :add}
         (#'tailor.diff/shift-change-map {2 :add 3 :add} 3)))
  (is (= {2 :add}
         (#'tailor.diff/shift-change-map {2 :add} 3))))

(deftest test-insert-line
  (let [changeset (atom (create-changeset ["a" "b" "c"]))]
    (swap! changeset insert-line "B" 2)
    (is (= ["a" "B" "b" "c"] (:lines @changeset)))
    (is (= {1 :add} (:change-map @changeset)))))

(deftest test-append-line
  (let [changeset (atom (create-changeset ["a" "b" "c"]))]
    (swap! changeset append-line "B" 2)
    (is (= ["a" "b" "B" "c"] (:lines @changeset)))
    (is (= {2 :add} (:change-map @changeset)))))

(deftest test-remove-line
  (let [changeset (atom (create-changeset ["a" "b" "c"]))]
    (swap! changeset remove-line 2)
    (is (= ["a" "b" "c"] (:lines @changeset)))
    (is (= {1 :remove} (:change-map @changeset)))))

(deftest test-change-line
  (let [changeset (atom (create-changeset ["a" "b" "c"]))]
    (swap! changeset change-line "B" 2)
    (is (= ["a" "b" "B" "c"] (:lines @changeset)))
    (is (= {2 :add 1 :remove} (:change-map @changeset)))))

(deftest test-create-changeset
  (is (= {:lines ["meat" "juice"] :offset 1 :change-map {}} (create-changeset ["meat" "juice"]))))

(deftest test-full-changeset-diff
  (let [changeset (atom (create-changeset ["a" "b" "c"]))]
    (swap! changeset change-line "B" 2)
    (is (= (changeset-diff @changeset)
           "@@ -1,3 +1,3 @@\n a\n-b\n+B\n c\n"))))

(deftest test-partial-changeset-diffs
  (let [changeset (atom (create-changeset ["a" "b" "c" "e" "f" "g" "i"]))]
    (swap! changeset change-line "B" 2)
    (swap! changeset insert-line "h" 7)
    (is (= (changeset-diff @changeset 1)
           "@@ -1,3 +1,3 @@\n a\n-b\n+B\n c\n@@ -6,2 +6,3 @@\n g\n+h\n i\n"))))
