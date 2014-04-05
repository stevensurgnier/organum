(ns organum.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

;; node constructors

(defn node [type] {:type type :content []})
(defn root [] (node :root))
(defn section [level name tags kw] (merge (node :section) {:level level :name name :tags tags :kw kw}))
(defn block [type qualifier] (merge (node :block) {:block-type type :qualifier qualifier}))
(defn drawer [] (node :drawer))
(defn line [type text] {:line-type type :text text})

(def rules
  (let [headline-re #"^(\*+)\s*(.*)$"
        headline-fn #(when-let [[_ pre text] (re-matches headline-re %)]
                       [pre text])
        pdrawer-begin-re #"^\s*:PROPERTIES:"
        pdrawer-end-re #"^\s*:END:"
        pdrawer-item-re #"^\s*:(.+):\s*(.+)\s*$"
        pdrawer-item-fn #(when-let [[_ prop v] (re-matches pdrawer-item-re %)]
                           [prop v])
        begin-block-re #"^#\+(BEGIN)_(\w+)\s*([\w\-]*)?.*"
        end-block-re #"^#\+(END)_(\w+)\s*([\w\-]*)?.*"
        block-fn (fn [re ln]
                   (when-let [[_ begin-end type qualifier] (re-matches re ln)]
                     [begin-end type qualifier]))
        def-list-re #"^\s*(-|\+|\s+[*])\s*(.*?)::.*"
        ordered-list-re #"^\s*\d+(\.|\))\s+.*"
        unordered-list-re #"^\s*(-|\+|\s+[*])\s+.*"
        metadata-re #"^\s*(CLOCK|DEADLINE|START|CLOSED|SCHEDULED):.*"
        table-sep-re #"^\s*\|[-\|\+]*\s*$"
        table-row-re #"^\\s*\\|.*"
        inline-example-re #"^\s*:\s.*"
        horiz-re #"^\s*-{5,}\s*$"
        comment-re #"^\s*# (.*)"
        comment-fn #(nth (re-matches comment-re %) 1)
        property-re #"^#\+(.+):\s*(.+)"
        property-fn #(when-let [[_ property v] (re-matches property-re %)]
                       [property v])]
    [:headline headline-fn
     :blank #(when (string/blank? %) "")
     :definition-list (partial re-matches def-list-re)
     :ordered-list (partial re-matches ordered-list-re)
     :unordered-list (partial re-matches unordered-list-re)
     :property-drawer-begin-block (partial re-matches pdrawer-begin-re)
     :property-drawer-end-block (partial re-matches pdrawer-end-re)
     :property-drawer-item pdrawer-item-fn
     :metadata (partial re-matches metadata-re)
     :begin-block (partial block-fn begin-block-re)
     :end-block (partial block-fn end-block-re)
     :comment comment-fn
     :property  property-fn
     :table-separator (partial re-matches table-sep-re)
     :table-row (partial re-matches table-row-re)
     :inline-example (partial re-matches inline-example-re)
     :horizontal-rule (partial re-matches horiz-re)
     :paragraph identity]))

(defn classify-line
  "Classify a line for dispatch to handle-line multimethod."
  ([ln] (classify-line rules ln))
  ([rules ln]
     ;; lazily evaluate each rule, returning the first non nil value
     (->> rules
          (partition 2)
          (map (fn [[k f]] [k (f ln)]))
          (filter second)
          first)))

(defn strip-tags
  "Return the line with tags stripped out and list of tags"
  [ln]
  (if-let [[_ text tags] (re-matches #"(.*?)\s*(:[\w:]*:)\s*$" ln)]
    [text (remove string/blank? (string/split tags #":"))]
    [ln nil]))

(defn strip-keyword
  "Return the line with keyword stripped out and list of keywords"
  [ln]
  (let [keywords-re #"(TODO|DONE)?"
        words (string/split ln #"\s+")]
    (if (re-matches keywords-re (words 0))
      [(string/triml (string/replace-first ln (words 0) "")) (words 0)]
      [ln nil])))

(defn parse-headline
  [prefix text]
  (let [[text tags] (strip-tags text)
        [text kw] (strip-keyword text)]
    (section (count prefix) text tags kw)))

(defn parse-block
  [begin-end type qualifier]
  (block type qualifier))

;; State helpers

(defn subsume
  "Updates the current node (header, block, drawer) to contain the specified
   item."
  [state item]
  (let [top (last state)
        new (update-in top [:content] conj item)]
    (conj (pop state) new)))

(defn subsume-top
  "Closes off the top node by subsuming it into its parent's content"
  [state]
  (let [top (last state)
        state (pop state)]
    (subsume state top)))

(defn handle-line
  ([state ln] (handle-line rules state ln))
  ([rules state ln]
     (let [[class value] (classify-line rules ln)]
       (case class
         :headline (conj state (apply parse-headline value))
         :begin-block (conj state (apply parse-block value))
         :end-block (subsume-top state)
         :property-drawer-begin-block (conj state (drawer))
         :property-drawer-end-block (subsume-top state)
         ;; default
         (subsume state (line class value))))))

(defn parse-seq
  ([lines] (parse-seq rules lines))
  ([rules lines]
     (reduce (partial handle-line rules) [(root)] lines)))

(defn parse-lines
  ([lines] (parse-lines rules lines))
  ([rules lines]
     (parse-seq rules (string/split-lines lines))))

(defn parse-file
  "Parse file (name / url / File) into (flat) sequence of sections.
   First section may be type :root, subsequent are type :section.
   Other parsed representations may be contained within the sections"
  ([f] (parse-file rules f))
  ([rules f]
     (with-open [rdr (io/reader f)]
       (parse-seq rules (line-seq rdr)))))
