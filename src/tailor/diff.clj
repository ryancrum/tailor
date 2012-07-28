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

(defn- net-change-count
  ([{change-map :change-map} up-to]
     (let [change-types (if (= :all up-to)
                          (vals change-map)
                          (vals (select-keys change-map
                                             (range up-to))))]
       (reduce +
               0
               (map #(case %
                           :add 1
                           :remove -1)
                    change-types))))
  ([changeset]
     (net-change-count changeset :all)))

(defn- convert-position [changeset target to-type]
  "Converts file line numbers to changeset array index numbers
   and back.

   i.e.
   (convert-position cs 5 :index) will return what the 0-based
      array index in changeset cs is.

   (convert-position cs 3 :line) will return what the line-number
      of the file post-changeset will be for the array index 3."
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
              (case to-type
                    :index (if (= current-line target)
                             current-index
                             (recur (inc current-index)
                                    (inc current-line)))
                    :line (if (>= current-index target)
                            current-line
                            (recur (inc current-index)
                                   (inc current-line))))))))))

(defn- line-index [changeset target-line]
  "Map a line number to an element index of :lines in changeset"
  (if (seq (:change-map changeset))
    (convert-position changeset target-line :index)
    (min (dec target-line)
         (dec (count (:lines changeset))))))

(defn- index-line [changeset index]
  "Map an index of :lines in changeset to a line number"
  (if (seq (:change-map changeset))
    (convert-position changeset index :line)
    (min (inc index)
         (count (:lines changeset)))))

(defn- cluster-changesets [changeset hunk-size]
  "Splits changeset into multiple changesets per hunk-size.

   For example:
   If hunk-size is 2 and changeset contains changes that
   are more than 2 lines apart, then this will return 2 changesets.

   If hunk-size is 2 and the changeset contains changes that are
   all within the immediate vicinity of each other, only one changeset
   will be return."
  (let [{change-map :change-map
         pre-offset :pre-offset
         post-offset :post-offset
         lines :lines} changeset]
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
                 [start end] hunk-range
                 post-offset (+ (index-line changeset start)
                                (dec post-offset)) ;; offset is 1-based
                 pre-offset (- post-offset
                               (net-change-count changeset start))]
             {:lines (lazy-subseq lines hunk-range)
              :pre-offset pre-offset
              :post-offset post-offset
              :change-map (apply merge
                                 (map
                                  (fn [k] {(- k start)
                                           (get change-map k)})
                                  key-list))}))
         clustered-keys))
      [])))

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
  "Inserts a line into changeset before the line at
   `before-line-number`."
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
  "Appends a line into changeset after the line at
   `after-line-number`."
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
  "Removes the line from changeset at `line-number`."
  (let [index (line-index changeset line-number)]
    (merge changeset
           {:change-map
            (merge (:change-map changeset)
                   {index :remove})})))

(defn change-line [changeset change-to line-number]
  "Modify `line-number` of changeset to `change-to`."
  (insert-line
   (remove-line changeset line-number)
   change-to
   line-number))

(defn changeset-diff
  "Create a diff string for a given changeset.

   If hunk-size is unspecified it will create a big diff for the
   entire changeset, otherwise it will split up the changeset into
   hunks using hunk-size as a minimum size guide and create diffs."
  ([changeset hunk-size]
     (reduce str
             (map changeset-diff
                  (cluster-changesets changeset hunk-size))))
  ([changeset]
     (if (seq (:change-map changeset))
       (let [size (count (:lines changeset))
             net-change (net-change-count changeset)
             initial-count (- size (change-count changeset :add))
             after-count (+ initial-count net-change)
             pre-offset (:pre-offset changeset)
             post-offset (:post-offset changeset)]
         (str
          (generate-hunk-header pre-offset initial-count
                                post-offset after-count)
          "\n"
          (formatted-hunk changeset)))
       "")))

(defn create-changeset
  "Create a changeset for the given set of base lines."
  [lines]
  {:lines lines
   :pre-offset 1
   :post-offset 1
   :change-map {}})

(defn diff-file-header
  "Generate a diff file header using the given path."
  [path]
  (str "--- " path "\n"
       "+++ " path "\n"))

(defn file-diff
  "Generate a full file diff for changeset with relative-path as the filename."
  ([changeset relative-path]
     (file-diff relative-path changeset 3))
  ([changeset relative-path hunk-size]
     (str
      (diff-file-header relative-path)
      (changeset-diff changeset hunk-size))))