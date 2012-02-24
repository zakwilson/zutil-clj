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

(defn make-adaptivepayments-query [user password key version method appid nvpm]
  (let [m (assoc nvpm
            :x-paypal-security-userid user
            :x-paypal-security-password password
            :x-paypal-security-signature key
            :x-paypal-service-version version
            :x-paypal-application-id appid
            :x-paypal-request-data-format "nv"
            :x-paypal-response-data-format "nv")
        q (str "method=" method "&"
               (map->query m))]
    q))