(ns parse-most-visited-websites.core
  (:require [net.cgrand.enlive-html :as html])
  (:gen-class))


; The base url that we're querying
(def ^:dynamic *base-url* "http://www.alexa.com/topsites/category;")

; The categories we intend to inspect
(def ^:dynamic *categories* [:Adult :Arts :Business :Computers :Games :Health :Home :Kids_and_Teens :News :Recreation :Reference :Regional :Science :Shopping :Society :Sports :World])

; The maximum number of pages per category, that we search
(def ^:dynamic *num-pages* 20)

; This is where we store our results
; It would have been easily possible to write this code without refs
; but I'm still learning Clojure and wanted to play around with refs
(def ^:dynamic *results* (atom []))

(defn pageize-page
  "creates a paged url for a category / page"
  [category page]
  (str *base-url* page "/Top/" (name category)))

(defn fetch-url [url]
  "Get the HTML for a page"
  (html/html-resource (java.net.URL. url)))

(defn urls-for-page
  "returns all urls in one particular page"
  [url]
  (let [data (fetch-url url)
        items (html/select data [:.site-listing :> :.desc-container :> :h2 :> :a])]
    (map #(first (:content %)) items)))
   
(defn process-category
  "process a category"
  [category]
  (dorun (map (fn [page]
         (let [page-url (pageize-page category page)
               local-results (urls-for-page page-url)]
           (println "gathered results for " page-url ", page" page ":")
           (println (clojure.string/join " " local-results))
           ; store the results
           (swap! *results* into local-results)
           ; and sleep a while to not overload the server
           (Thread/sleep 1000))
         ) (range 0 *num-pages*))))

(defn write-out-atom
  [atom-val]
  (let [atom-string (clojure.string/join "\n" @*results*)]
    (spit "results.txt" atom-string)))

(defn process-categories
  "process all categories"
  [categories]
  (dorun (map (fn [category]
         (process-category category)
           ) categories)))

(defn process
  "do the processing"
  []
  (process-categories *categories*)
  (write-out-atom *results*))

(defn -main
  [& args]
  ;(process)
  )


