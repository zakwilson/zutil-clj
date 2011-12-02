(ns zutil.db
  (:refer-clojure
   :exclude [take drop sort distinct conj! disj! compile case spit extend])
  (:use zutil.util clojureql.core clojureql.predicates urs.common clojure.contrib.duck-streams clj-time.core)
  (:require [clojure.string :as s]
            [clojure.contrib.sql :as sql]))

