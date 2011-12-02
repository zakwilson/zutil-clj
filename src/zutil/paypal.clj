(ns zutil.paypal
  (:refer-clojure :exclude [get])
  (:use clj-http.client zutil.web))

(defn make-paypal-query [user password key version method nvpm]
  (let [m (assoc nvpm
            :user user
            :pwd password
            :signature key
            :version version)
        q (str "method=" method "&"
               (map->query m))]
    q))
