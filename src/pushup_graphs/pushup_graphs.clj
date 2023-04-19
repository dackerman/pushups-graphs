(ns pushup-graphs.pushup-graphs
  {:nextjournal.clerk/visibility {:code :hide :result :hide}}
  (:require
   [nextjournal.clerk :as clerk]
   [clojure.data.csv :as csv]
   [clojure.string :as string]
   [org.httpkit.client :as http]))


(def now (java.time.OffsetDateTime/now))


(defn download-google-sheet! []
  (:body @(http/get "https://docs.google.com/spreadsheets/d/1uBn9diObqs0Sz-ouS-iP63IZ5hu5GcsL73G5dVQxWqo/export?format=csv&id=1uBn9diObqs0Sz-ouS-iP63IZ5hu5GcsL73G5dVQxWqo&gid=1885606659")))


(defn format-full [d]
  (.format java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME d))


(defn format-short-date [d]
  (.format (java.time.format.DateTimeFormatter/ofPattern "MMM dd") d))


(defn parse-pushup-number [pushups-string]
  (or (parse-long (string/replace pushups-string "[^0-9]" "")) 0))


(defn parse-pushup-row [columns]
  (let [current-day (dec (.getDayOfMonth now))
        pushups (map parse-pushup-number (take 30 (rest columns)))]
    {:name (first columns)
     :pushups pushups
     :yesterday-total (reduce + 0 (take (dec current-day) pushups))
     :today-total (reduce + 0 (take current-day pushups))
     :total (reduce + 0 pushups)}))


(defn parse-pushups-csv [csv-input]
  (doall
   (->> (csv/read-csv csv-input)
        (drop 4)
        (map parse-pushup-row)
        (filter (comp not string/blank? :name))
        (filter #(> (:total %) 0)))))


(defn convert-to-cumulative [pushups]
  (reductions + 0 pushups))


(defn chart-pushups [data]
  (let [days-to-display (.getDayOfMonth now)]
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



(def pushups-data
  (->> (parse-pushups-csv (download-google-sheet!))
       (sort-by :yesterday-total)
       (reverse)
       (map-indexed (fn [yesterday-rank m] (assoc m :yesterday-rank yesterday-rank)))
       (sort-by :today-total)
       (reverse)
       (map-indexed (fn [today-rank m] (assoc m :today-rank today-rank)))
       (sort-by :total)
       (reverse)))

(def partitioned-by-rank (partition 10 10 [] pushups-data))


{:nextjournal.clerk/visibility {:result :show}}


(clerk/md
 (format
  "# Pushup rankings as of %s"
  (format-short-date now)))


(clerk/md "### Biggest improvement since yesterday")
(clerk/table
 (->> pushups-data
      (map #(assoc % :rank-diff (- (:today-rank %) (:yesterday-rank %))))
      (filter #(< (:rank-diff %) 0))
      (sort-by :rank-diff)
      (take 10)
      (map (fn [{:keys [name yesterday-rank today-rank rank-diff]}]
             {"Name" name
              "Diff" (str "+ " (- rank-diff))
              "Yesterday's Rank" yesterday-rank
              "Today's Rank" today-rank}))))


(clerk/md "### Largest # of pushups yesterday")
(clerk/table
 (->> pushups-data
      (map #(assoc % :pushups-done-yesterday (nth (:pushups %) (- (.getDayOfMonth now) 2) 0)))
      (sort-by :pushups-done-yesterday)
      (reverse)
      (take 10)
      (map (fn [{:keys [name pushups-done-yesterday]}]
             {"Name" name
              "Pushups" pushups-done-yesterday}))))


(clerk/md "## Ranking graphs
The graphs below show cumulative pushups done. Each graph shows 10 people at once, so keep scrolling or use Ctrl+f to find your name.")
(clerk/html
 {:nextjournal/width :full}
 (into
  [:div]
  (for [[rank people] (map-indexed vector partitioned-by-rank)]
    [:div
     [:h2 (format "Ranks %d - %d" (inc (* 10 rank)) (+ 10 (* 10 rank)))]
     (chart-pushups people)])))

(clerk/md (str "Last run: " (format-full now)))
