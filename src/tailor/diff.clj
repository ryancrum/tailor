(ns tailor.diff)

(defn- generate-hunk-header
  "Generates a hunk header given a starting line-number and the line counts"
  [offset-before count-before offset-after count-after]
  (str "@@ -" offset-before "," count-before
       " +" offset-after "," count-after " @@"))

(defn- clamp-window-to-range
  "Clamps a range to actual list bounds"
  [[start end] window-size maximum]
  [(max 0 (- start window-size))
   (min maximum (+ end window-size))])

(defn- lazy-subseq
  "Create a lazy subsequence of a sequence"
  [s [start end]]
  (take (inc (- end start))
        (drop start s)))

(defn- formatted-hunk
  [changeset]
  (let [change-map (:change-map changeset)]
    (loop [current-index 0
           lines (:lines changeset)
           output []]
      (if (seq lines)
        (let [change-type (get change-map current-index)
              prefix
              (if change-type
                (case change-type
                      :add "+"
                      :remove "-")
                " ")]
          (recur (inc current-index)
                 (drop 1 lines)
                 (conj output
                       (str prefix (first lines) "\n"))))
        (apply str output)))))


(defn- cluster-seq [s dist]
  "Cluster elements of s with a difference more than dist"
  (loop [remaining s
         cur []
         ret []]
    (let [lst (last cur)
          nxt (first remaining)]
      (if (seq remaining)
        (if (and lst
                 (> (- nxt lst) dist))
          (recur
           (rest remaining)
           [nxt]
           (conj ret cur))
          (recur
           (rest remaining)
           (conj cur nxt)
           ret))
        (conj ret cur)))))

(defn- change-count [{change-map :change-map} change-type]
  (count (filter #(= % change-type)
                 (vals change-map))))

(defn- net-change-count [{change-map :change-map}]
  (let [change-types (vals change-map)]
    (reduce +
           0
           (map #(case %
                       :add 1
                       :remove -1)
                change-types))))

(defn- line-index [changeset target-line]
  "Map a line number to an element index of :lines in changeset"
  (if (seq (:change-map changeset))
    (let [line-count (count (:lines changeset))
          changemap (:change-map changeset)]
      (loop [current-index 0
             current-line 1]
        (let [op (get changemap current-index)]
          (if (>= current-index line-count)
            (dec line-count)
            (if (= :remove op)
              (recur (inc current-index)
                     current-line)
              (if (= current-line target-line)
                current-index
                (recur (inc current-index)
                       (inc current-line))))))))
    (min (dec target-line)
         (dec (count (:lines changeset))))))

(defn- cluster-changesets [{change-map :change-map
                               offset :offset
                               lines :lines}
                           hunk-size]
  (if (seq change-map)
    (let [change-keys (sort (keys change-map))
          clustered-keys (cluster-seq change-keys (inc hunk-size))]
      (map
       (fn [key-list]
         (let [hunk-range
               (clamp-window-to-range
                [(first key-list) (last key-list)]
                hunk-size
                (count lines))
               [start end] hunk-range]
           {:lines (lazy-subseq lines hunk-range)
            :offset (+ start offset)
            :change-map (apply merge
                               (map
                                (fn [k] {(- k start)
                                         (get change-map k)})
                                key-list))}))
       clustered-keys))
    []))

(defn- shift-change-map [changemap at]
  "Shift values of changemap up where line numbers > at"
  (let [line-numbers (sort (keys changemap))
        unchanged-line-numbers (filter #(< % at) line-numbers)
        changed-line-numbers (filter #(>= % at) line-numbers)
        base-changemap (select-keys changemap unchanged-line-numbers)]
    (reduce
     (fn [acc curr]
       (assoc acc (inc curr) (get changemap curr)))
     base-changemap
     changed-line-numbers)))

(defn insert-line [changeset line before-line-number]
  (let [index (line-index changeset before-line-number)
        lines (:lines changeset)]
    (merge changeset
           {:lines (concat
                    (take index lines)
                    [line]
                    (drop index lines))
            :change-map (assoc
                            (shift-change-map (:change-map changeset)
                                              index)
                          index
                          :add)})))

(defn append-line [changeset line after-line-number]
  (let [index (inc (line-index changeset after-line-number))
        lines (:lines changeset)]
    (merge changeset
           {:lines (concat
                    (take index lines)
                    [line]
                    (drop index lines))
            :change-map (assoc
                            (shift-change-map (:change-map changeset)
                                              index)
                          index
                          :add)})))

(defn remove-line [changeset line-number]
  (let [index (line-index changeset line-number)]
    (merge changeset
           {:change-map
            (merge (:change-map changeset)
                   {index :remove})})))

(defn change-line [changeset change-to line-number]
  (insert-line
   (remove-line changeset line-number)
   change-to
   line-number))

(defn changeset-diff
  "Create a diff string for a given changeset."
  ([changeset hunk-size]
     ;; If hunk-size is unspecified it will create a big diff for the
     ;; entire changeset, otherwise it will split up the changeset into
     ;; hunks using hunk-size as a minimum size guide and create diffs.
     (reduce str
             (map changeset-diff
                  (cluster-changesets changeset hunk-size))))
  ([changeset]
     (if (seq (:change-map changeset))
       (let [size (count (:lines changeset))
             net-change (net-change-count changeset)
             initial-count (- size (change-count changeset :add))
             after-count (+ initial-count net-change)
             offset (:offset changeset)]
         (str
          (generate-hunk-header (inc offset) initial-count
                                (inc offset) after-count)
          "\n"
          (formatted-hunk changeset)))
       "")))

(defn create-changeset
  "Create a changeset for the given set of base lines."
  [lines]
  {:lines lines
   :offset 0
   :change-map {}})

(defn diff-file-header
  "Generate a diff file header using the given relative path."
  [relative-path]
  (str "--- " relative-path "\n"
       "+++ " relative-path "\n"))

(defn file-diff
  "Generate a full file diff."
  ([relative-path changeset]
     (file-diff relative-path changeset 3))
  ([relative-path changeset hunk-size]
     (str
      (diff-file-header relative-path)
      (changeset-diff changeset hunk-size))))