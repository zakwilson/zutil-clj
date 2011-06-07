(ns zutil.web
  (:use hiccup.core))

(defn hstr [& args]
  (apply str (map escape-html args)))