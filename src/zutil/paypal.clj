(ns zutil.paypal
  (:refer-clojure :exclude [get])
  (:use clj-http.client zutil.web))

(defn make-paypal-query [user password key version method nvpm & [appid]]
  (let [m (assoc nvpm
            :user user
            :pwd password
            :signature key
            :version version
            :x-paypal-application-id appid)
        q (str "method=" method "&"
               (map->query m))]
    q))