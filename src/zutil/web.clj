(ns zutil.web
  (:refer-clojure :exclude [get])
  (:use hiccup.core hiccup.form-helpers ring.util.codec clj-http.client
        [zutil util map])
  (:require ring.middleware.params))

(defn hstr [& args]
  (apply str (map escape-html args)))

(defmacro defhandler [name args & body]
  `(defn ~name [~'req]
     {:status 200
      :body ~@body}))

(defn hidden [name value]
  [:input {:type "hidden" :name name :id name :value value}])

(defn action-button [url name value text]
  (form-to [:post url]
           [:input {:type "hidden" :name name :id name :value value}]
           (submit-button text)))

(defn map->query [m]
  (loop [pairs (into [] m)
         retstring ""]
    (if (seq pairs)
      (let [[[k v]] pairs]
        (recur (rest pairs)
               (str retstring
                    "&"
                    (url-encode (name k))
                    "="
                    (url-encode (str v)))))
      (if (> (count retstring) 0)
        (subs retstring 1)
        ""))))

(defn query->map [q]
  (keyword-keys
   (@#'ring.middleware.params/parse-params q "UTF-8")))

(defn input 
  ([text id] (input text id :text))
  ([text id type]
     [:span
      (label id (str text))
      [:input {:name id :id id :type (name type)}]]))

(defn select-option [kv & [selected]]
  [:option {:value (first kv)
            :selected (when (= selected (first kv))
                        "selected")}
   (name (second kv))])

(defn select-box [name options & [selected]]
  [:select {:name name :id name}
   (map #(select-option % selected) options)])


(defn strip-price [price]
  (when price
    (.replaceAll price "[^0-9\\.]*" "")))

(def state-abbrevs
     [["Alabama" "AL"]
      ["Alaska" "AK"]
      ["Arizona" "AZ"]
      ["Arkansas" "AR"]
      ["California" "CA"]
      ["Colorado" "CO"]
      ["Conneticut" "CT"]
      ["Delaware" "DE"]
      ["Florida" "FL"]
      ["Georgia" "GA"]
      ["Hawaii" "HI"]
      ["Idaho" "ID"]
      ["Illinois" "IL"]
      ["Iowa" "IA"]
      ["Kansas" "KS"]
      ["Kentucky" "KY"]
      ["Louisiana" "LA"]
      ["Maine" "ME"]
      ["Maryland" "MD"]
      ["Massachusetts" "MA"]
      ["Minnesota" "MN"]
      ["Mississippi" "MS"]
      ["Missouri" "MO"]
      ["Montana" "MT"]
      ["Nebraska" "NE"]
      ["Nevada" "NV"]
      ["New Hampshire" "NH"]
      ["New Jersey" "NJ"]
      ["New Mexico" "NM"]
      ["New York" "NY"]
      ["North Carolina" "NC"]
      ["North Dakota" "ND"]
      ["Ohio" "OH"]
      ["Oklahoma" "OK"]
      ["Oregon" "OR"]
      ["Pennsylvania" "PA"]
      ["Rhode Island" "RI"]
      ["South Carolina" "SC"]
      ["Tennessee" "TN"]
      ["Texas" "TX"]
      ["Utah" "UT"]
      ["Vermont" "VT"]
      ["Virginia" "VA"]
      ["Washington" "WA"]
      ["West Virginia" "WV"]
      ["Wisconsin" "WI"]
      ["Wyoming" "WY"]])

(def rstate-abbrevs
     (map reverse state-abbrevs))

(def month-pairs
     [[1 "January"]
      [2 "February"]
      [3 "March"]
      [4 "April"]
      [5 "May"]
      [6 "June"]
      [7 "July"]
      [8 "August"]
      [9 "September"]
      [10 "October"]
      [11 "November"]
      [12 "December"]])

(defn link [url content options]
     [:a (assoc options :href url)
      content])

(def old-url-encode url-encode)
(def old-url-decode url-decode)

(defn url-encode-properly [unencoded & [encoding]]
  "Properly urlencode unencoded, because the author of java.net.URLEncoder doesn't know what he's doing"
  (.replaceAll (old-url-encode unencoded encoding) "\\+" "%20"))

(defn url-decode-properly [encoded & [encoding]]
  "Properly urldecode encoded, because the author of java.net.URLEncoder doesn't know what he's doing"
  (old-url-decode (.replaceAll encoded "\\+" "%2b")))