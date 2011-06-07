(ns zutil.util
;  (:use clojure.contrib.seq-utils :only (position partition-all))
  (:import (java.io File)))

(defn position
  ([x coll] (position x coll 0))
  ([x coll n]
     (cond (nil? (first coll)) nil
           (= x (first coll )) n
           true (recur x (drop 1 coll) (inc n)))))

(defn rposition [x coll]
  (let [len (count coll)
        pos (position x (reverse coll))]
    (when pos (- len pos))))

(defn member? [x coll]
  (when (seq (filter #(= % x) coll))
    x))


(defn append-pathname [pathname x]
  (let [sep (rposition \. pathname)]
  (str (subs pathname 0 (dec sep))
       x
       \.
       (subs pathname sep))))

(defn unique-filename
  ([pathname] (unique-filename pathname pathname 1))
  ([pathname try-path n]
     (if (.exists (File. try-path))
       (recur pathname (append-pathname pathname (str \- n)) (inc n))
       try-path)))

(defn identity* [x y]
  (identity y))

(defn get-dir [d]
  (map (memfn getPath)
       (filter (memfn isFile)
               (.listFiles (File. d)))))

(defn zpmap [f coll]
  (apply concat
         (pmap #(doall (map f %))
              (partition-all (Math/ceil (/ (count coll)
                                             (float (+ 2 (.availableProcessors
                                                          (Runtime/getRuntime))))))
                             coll))))


;; third-party - no copyright assignment for make-thumbnail

(defn make-thumbnail [filename new-filename width]
  (let [img (javax.imageio.ImageIO/read (j/as-file filename))
        imgtype (java.awt.image.BufferedImage/TYPE_INT_RGB)
        width (min (.getWidth img) width)
        height (* (/ width (.getWidth img)) (.getHeight img))
        simg (java.awt.image.BufferedImage. width height imgtype)
        g (.createGraphics simg)]
    (.drawImage g img 0 0 width height nil)
    (.dispose g)
    (javax.imageio.ImageIO/write simg "jpg" (j/as-file new-filename))))