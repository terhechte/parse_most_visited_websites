(ns parse-most-visited-websites.analyze
  (:import [java.net URI URL])
  ; We use incanter for some plotting
  (:use incanter.core)
  (:use incanter.charts)
  (:gen-class))

; We can test this for various analysis methods:
; :domain -> the domain ending (de, com, org ...)
; :first -> the first letter of the domain name
; :length -> the length of the domain name (amount of letters)
; :length4 -> the length of the domain name (amount of letters) / 4
; :firstbucket -> first letter based on a bucket function
(def ^:dynamic *analysis-method* :domain)

(defn cleanse-url
  "remove http, https www*."
  [urlstring]
  (-> (str urlstring)
      (clojure.string/replace #"https?\:\/\/" "")
      (clojure.string/replace #"www[0-9]?\." "")
      (clojure.string/replace #"^m\." "") ;mobile
      ))

(defn read-domains
  "read and return domain data as list"
  []
  (-> (slurp "results.txt")
      (clojure.string/split #"\n")))


(defn- std-dev [samples]
  "calculate the standard deviation"
  (let [n (count samples)
	mean (/ (reduce + samples) n)
	intermediate (map #(Math/pow (- %1 mean) 2) samples)]
    (Math/sqrt 
     (/ (reduce + intermediate) n))))    

(defn- get-tld
  "get the tld for an url"
  [url]
  (last (clojure.string/split (.getHost url) #"\.")))

(defn- first-bucket
  "a bucketing where some letters land in group one, some in group 2, etc"
  [string]
  (let [c (subs string 0 1)]
    (cond
     (re-find #"[s]" c) 0
     (re-find #"[ak]" c) 1
     (re-find #"[bl]" c) 2
     (re-find #"[cj]" c) 3
     (re-find #"[de]" c) 4
     (re-find #"[fg]" c) 5
     (re-find #"[hiv]" c) 6
     (re-find #"[mr]" c) 7
     (re-find #"[np]" c) 8
     (re-find #"[txyzo]" c) 9
     :else 10))) ; rest, i.e. uw0123456789q

(defn- group-function
  "the function we use to group the results"
  [value]
  (case *analysis-method*
    :domain (get-tld value)
    :first (subs (cleanse-url value) 0 1)
    :firstbucket (first-bucket (cleanse-url value))
    :length (count (cleanse-url value))
    :length4 (int (/ (count (cleanse-url value)) 4))))

(defn- convert-domains
  "convert the domain strings into full urls"
  [urlstrings]
  (map
   (fn [urlstring]
     (let [trimmedstring (clojure.string/trim urlstring)
           repairedstring (if-not (re-find #"^https?\:\/\/" trimmedstring)
                            (str "http://" trimmedstring)
                            trimmedstring)
           url (URL. repairedstring)]
       url)
     ) urlstrings))

(defn group-domains
  "group the domains based on the chosen method and calculate distributions"
  [method]
  (binding [*analysis-method* method]
    (let [domains (read-domains)
          converted (convert-domains domains)
          grouped (group-by group-function converted)
          f (fn [x] [(count x) (float (/ (count x) (count domains)))])]
      ; replace the value with a count of the domains, not the domains
      (into {} (for [[k v] grouped]
                 [k (f v)])))))

(defn chart-results
  "chart the results of the grouping"
  [groups]
  (let [results (into [] (for [[k v] groups] [k (first v)]))]
    results
    (bar-chart (map first results) (map last results)
               :title (str "Deviation " (std-dev (-> groups vals first)))
               :x-label "Groups"
               :y-label "Entries")))

(defn present-results
  "show the standard variation for the results"
  ([groups]
     (present-results groups nil))
  ([groups detailed]
     (when detailed 
       (dorun (for [[k v] groups]
                (println k "\t:\t\t" (first v) "\t #" (last v))))
       (println "---------------------------"))
     (println "Amount of groups: " (count groups))
     (println "Deviation: " (std-dev (-> groups vals first)))))
