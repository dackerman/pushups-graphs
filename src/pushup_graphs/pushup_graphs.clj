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
  (let [pushups (map parse-pushup-number (take 30 (rest columns)))]
    {:name (first columns)
     :pushups pushups
     :total (reduce + 0 pushups)}))


(defn parse-pushups-csv [csv-input]
  (doall
   (->> (csv/read-csv csv-input)
        (drop 4)
        (map parse-pushup-row)
        (filter (comp not string/blank? :name))
        (filter #(> (:total %) 0))
        (sort-by :total)
        (reverse))))


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


(def pushups-data (parse-pushups-csv (download-google-sheet!)))


(def partitioned-by-rank (partition 10 10 [] pushups-data))


{:nextjournal.clerk/visibility {:result :show}}

(clerk/md
 (format
  "# Pushup rankings as of %s

Graphs show groups of 10 people at once, in order of how many they've done so far.

Keep scrolling to find yourself in the rankings, or Ctrl+f to search for your name as it appears in the spreadsheet.
" (format-short-date now)))

(clerk/html
 {:nextjournal/width :full}
 (into
  [:div]
  (for [[rank people] (map-indexed vector partitioned-by-rank)]
    [:div
     [:h2 (format "Ranks %d - %d" (inc (* 10 rank)) (+ 10 (* 10 rank)))]
     (chart-pushups people)])))

(clerk/md (str "Last run: " (format-full now)))
