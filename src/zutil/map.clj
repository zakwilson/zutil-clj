;   Copyright (c) Zak Wilson. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns zutil.map
  (:use zutil.util))

(defn lookup-with [default k a-map]
  (let [found (a-map k)]
    (if found
      found
      default)))

(defn intersection-with [f m1 m2]
  (loop [ks (keys m1)
         ret (transient {})]
    (if (seq ks)
      (let [k (first ks)
            a (m1 k)
            b (m2 k)]
        (if (and a b)
          (recur (rest ks)
                 (assoc! ret k (f a b)))
          (recur (rest ks) ret)))
      (persistent! ret))))

(defn intersection [m1 m2]
  (intersection-with (fn [a b] a) m1 m2))

(defn difference [m1 m2]
  (loop [ks (keys m1)
         ret (transient {})]
    (if (seq ks)
      (let [k (first ks)
            a (m1 k)
            b (m2 k)]
        (if (and a (not b))
          (recur (rest ks)
                 (assoc! ret k a))
          (recur (rest ks) ret)))
      (persistent! ret))))

(defn union-with
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping(s)
  from the latter (left-to-right) will be combined with the mapping in
  the result by calling (f val-in-result val-in-latter)."
  [f default & maps]
  (when (some identity maps)
    (let [merge-entry (fn [m e]
			(let [k (key e) v (val e)]
			  (if (contains? m k)
			    (assoc m k (f (m k) v))
			    (assoc m k (f default v)))))
          merge2 (fn [m1 m2]
		   (reduce merge-entry (or m1 {}) (seq m2)))]
      (reduce merge2 maps))))

(defn keyword-keys [m]
  (into {} (map (fn [kv] [(keyword (key kv)) (val kv)])
                m)))

(defn map-map [f m]
  (let [kv-f (fn [kv]
               [(key kv) (f (val kv))])]
    (into {} (map kv-f m))))

(defn map-keys [f m & [ks]]
  "Returns the map with f applied to the value associated with every key in ks"
  (let [submap (select-keys m ks)
        subresult (map-map f submap)]
    (merge m subresult)))

(comment
(defn map-keys [f m & [ks]]
  (loop [new-map (transient {}) ks ks]
    (if (empty? ks)
      (persistent! new-map)
      (let [[k & ks] ks]
        (recur (assoc! new-map k (f (m k))) ks)))))
)

(defn underscoreize [kw]
  (keyword (.replaceAll (name kw)
                        "-"
                        "_")))

(defn dashize [kw]
  (keyword (.replaceAll (name kw)
                        "_"
                        "-")))

(defn dash->underscore [m]
  (into {}
        (map (fn [x]
               [(underscoreize (key x))
                (val x)])
             m)))

(defn underscore->dash [m]
  (into {}
        (map (fn [x]
               [(dashize (key x))
                (val x)])
             m)))

(defn reject-keys [map keyseq]
  (let [good-keys (filter #(not (member? % keyseq))
                          (keys map))]
    (select-keys map good-keys)))