(ns pushup-graphs.pushup-graphs
  {:nextjournal.clerk/visibility {:code :hide :result :hide}}
  (:require
   [nextjournal.clerk :as clerk]
   [clojure.data.csv :as csv]
   [clojure.java.io :as io]
   [clojure.string :as string]))


(def now (java.time.OffsetDateTime/now))

^::clerk/no-cache
(def downloaded-google-sheet-csv
 "/home/david/Downloads/Apr Pushups Challenge  - Apr2023.csv")


(defn format-full [d]
  (.format java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME d))


(defn format-short-date [d]
  (.format (java.time.format.DateTimeFormatter/ofPattern "MMM dd") d))


(defn parse-pushup-number [pushups-string]
  (or (parse-long (string/replace pushups-string "[^0-9]" "")) 0))


(defn parse-pushup-row [columns]
  (let [pushups (map parse-pushup-number (take 30 (rest columns)))]
    {:name (first columns)
     :pushups pushups
     :total (reduce + 0 pushups)}))


(def pushups-data
  (with-open [reader (io/reader (io/file downloaded-google-sheet-csv))]
    (doall
     (->> (csv/read-csv reader)
          (drop 4)
          (map parse-pushup-row)
          (filter (comp not string/blank? :name))
          (sort-by :total)
          (reverse)))))


(defn convert-to-cumulative [pushups]
  (reductions + 0 pushups))


(defn chart-pushups [data]
  (let [days-to-display (inc (.getDayOfMonth now))]
    (clerk/plotly
     {:nextjournal/width :full}
     {:config {}

      :layout
      {:height 1000
       :yaxis {}}

      :data
      (for [{:keys [name pushups]} data]
        {:name name
         :x (take days-to-display (map #(str "Apr " %) (range 1 31)))
         :y (take days-to-display (convert-to-cumulative pushups))})})))


(defn between
  ([lower] (between lower 100000000))
  ([lower upper]
   #(and (>= (:total %) lower)
         (< (:total %) upper))))


{:nextjournal.clerk/visibility {:result :show}}

(clerk/md (str "# Pushup rankings as of " (format-short-date now)))

(clerk/md "## 2000+ pushups completed")
(chart-pushups (filter (between 2000) pushups-data))

(clerk/md "## 1000-2000 pushups completed")
(chart-pushups (filter (between 1000 2000) pushups-data))

(clerk/md "## 500-1000 pushups completed")
(chart-pushups (filter (between 500 1000) pushups-data))

(clerk/md "## 0-500 pushups completed")
(chart-pushups (filter (between 0 500) pushups-data))

(clerk/md (str "Last run: " (format-full now)))
