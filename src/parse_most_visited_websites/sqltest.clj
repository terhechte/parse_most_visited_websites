(ns parse-most-visited-websites.sqltest
  (:require [parse-most-visited-websites.analyze :as analyze])
  (:require [clojure.string :as st] )
  (:import [java.net URI URL])
  (:gen-class))

(defn write-insert-calls
  "write the insert calls for table into a file"
  [table]
  (->> (map #(format "INSERT INTO %s (url) VALUES (E'%s');" table (analyze/cleanse-url %))
            (analyze/read-domains))
      (st/join "\n")
      (spit (format "generate-urls-%s.sql" table))))

(defn generate-urls []
  (map write-insert-calls ["urls" "alt_urls"]))

(defn generate-table-structure-queries
  "we will simply generate our table structure in clojure, as we're lazy"
  [query_template]
  (let [mappings ["s" "ak" "bl" "cj" "de" "fg" "hiv" "mr" "np" "txzyo" "uw0123456789q"]]
    (map (fn [ts]
           (format query_template ts (st/join ", " (map int (seq ts))))
           ) mappings)))

(defn generate-tables
  []
  (st/join "\n" (generate-table-structure-queries "CREATE TABLE urls_%s (CHECK (ascii(url) in (%s))) INHERITS (urls);")))

(defn generate-trigger-function
  []
  (st/join "\n" (generate-table-structure-queries "IF (ascii(NEW.url) in (%2$s)) THEN
       INSERT INTO urls_%1$s VALUES (NEW.*);")))

