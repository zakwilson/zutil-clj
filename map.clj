;   Copyright (c) Zak Wilson. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns zutil.map)

(defn lookup-with [default k a-map]
  (let [found (a-map k)]
    (if found
      found
      default)))

(defn intersection-with [f m1 m2]
  (loop [ks (keys m1);(set (concat (keys m1) (keys m2)))
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
  (loop [ks (keys m1);(set (concat (keys m1) (keys m2)))
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
