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
                    (url-encode v))))
      (subs retstring 1))))

(defn query->map [q]
  (keyword-keys
   (@#'ring.middleware.params/parse-params q "UTF-8")))

(defn input 
  ([text id] (input text id :text))
  ([text id type]
     [:span
      (label id (str text))
      [:input {:name id :id id :type (name type)}]]))

(defn select-option [kv]
  [:option {:value (key kv)}
   (name (val kv))])

(defn select-box [name options]
  [:select {:name name :id name}
   (map select-option options)])