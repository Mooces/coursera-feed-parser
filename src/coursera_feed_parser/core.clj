(ns coursera-feed-parser.core
  (:require [clj-http.client :as client]
            [clojure.data.json :as json]
            [clojure.data.csv :as csv]
            [clj-time.core :as time]
            [clj-time.format :as format]
            [clojure.string :as string]
            [clojure.java.io :as io])
  (:use [slingshot.slingshot :only [throw+ try+]]))

(def months ["January" "February" "March" "April" "May" "June"
             "July" "August" "September" "October" "November" "December"])

(def months-exceptional {"Feb" "02"})

(def month-num (merge
                (apply hash-map
                       (interleave months
                                   (map (fn [num] (format "%02d" num))
                                        (range 1 13))))
                months-exceptional))

(def json-course-list-url
  "http://www.coursera.org/maestro/api/topic/list?full=1")

(defn parse-date [date-string]
  (try+
   (let [[day month year] (string/split date-string #" ")
         date-formatter (format/formatters :basic-date)]
     (if (and (seq day) (seq month) (seq year))
       (format/parse date-formatter (str year (month-num month) (format "%02d" (Integer/parseInt day))))
       nil))
   (catch java.lang.NumberFormatException e
     (println "Could not parse date:" date-string))))

(defn get-dates [courses]
  (loop [courses courses dates []]
    (if (seq courses)
      (let [course (first courses)
            start-date (parse-date (:start_date_string course))]
        (if (nil? start-date)
          (recur (rest courses) dates)
          (let [[weeks-num _] (string/split (:duration_string course) #" ")]
            (if (seq weeks-num)
              (let [end-date (time/plus start-date (time/weeks (Integer/parseInt weeks-num)))]
                (recur (rest courses) (conj dates [start-date end-date])))
              (recur (rest courses) dates)))))
      dates)))

(defn create-course-dates [name dates]
  (let [date-formatter (format/formatter "MM/dd/yy")]
    (loop [dates dates results []]
      (if (seq dates)
        (let [[start-date end-date] (first dates)
              formatted-start-date (format/unparse date-formatter start-date)
              formatted-end-date (format/unparse date-formatter end-date)]
          (recur (rest dates)
                 (cons [name formatted-start-date formatted-end-date "True"] results)))
        results))))

(defn get-courses-dates [course-feed]
  (loop [courses course-feed course-dates []]
    (if (seq courses)
      (let [course (first courses)
            dates (get-dates (:courses course))
            name (:name course)]
        (recur (rest courses)
               (concat course-dates (create-course-dates name dates))))
      course-dates)))

(defn save-csv [courses-dates]
  (with-open [out-file (io/writer "calendar.csv")]
    (csv/write-csv out-file [["Subject", "Start Date", "End Date", "All Day Event"]])
    (csv/write-csv out-file courses-dates)))

(defn -main [& args]
  (let [body (json/read-json (:body (client/get json-course-list-url)))]
    (save-csv (get-courses-dates body))))
