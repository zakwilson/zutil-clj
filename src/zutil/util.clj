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
  (when (some #(= % x) coll)
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

(defn get-filenames [d]
  (map (memfn getName)
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

(defn make-thumbnail 
  ([source new-filename width]
     (make-thumbnail source new-filename width width))
  ([source new-filename width height]
     (spit "img" source)
     (let [img (javax.imageio.ImageIO/read source)
           imgtype (java.awt.image.BufferedImage/TYPE_INT_RGB)
           orig-width (.getWidth img)
           orig-height (.getHeight img)
           width (min orig-width width)
           height (min orig-height
                       height
                       (* (/ width orig-width) orig-height))
           width (min width ;round and round we go!
                      (* (/ height orig-height) orig-width))
           simg (java.awt.image.BufferedImage. width height imgtype)
           g (.createGraphics simg)]
       (.drawImage g img 0 0 width height nil)
       (.dispose g)
       (javax.imageio.ImageIO/write simg "jpg" (File. new-filename)))))

(defn integer [x]
  (cond (string? x) (Integer/parseInt x)
        (integer? x) x
        (number? x) (int x)
        true (throw (NumberFormatException.))))

(defn maybe-integer [x]
  (try (integer x)
       (catch Exception _
         nil)))

(defn decimal [x]
  (cond (string? x) (Double/parseDouble x)
        (float? x) x
        (number? x) (float x)
        true (throw (NumberFormatException.))))

(defn maybe-decimal [x]
  (try (decimal x)
       (catch Exception _
         nil)))

(def VALID-CHARS
     (map char (concat (range 48 58) ; 0-9
                       (range 66 91) ; A-Z
                       (range 97 123)))) ; a-z

(defn random-char []
  (nth VALID-CHARS (rand (count VALID-CHARS))))

(defn random-str [length]
  (apply str (take length (repeatedly random-char))))

;; from c.c.seq - Copyright Stuart Sierra, EPL

(defn separate
  "Returns a vector:
   [ (filter f s), (filter (complement f) s) ]"
  [f s]
  [(filter f s) (filter (complement f) s)])

(defn indexed
  "Returns a lazy sequence of [index, item] pairs, where items come
  from 's' and indexes count up from zero.

  (indexed '(a b c d))  =>  ([0 a] [1 b] [2 c] [3 d])"
  [s]
  (map vector (iterate inc 0) s))

(defn positions
  "Returns a lazy sequence containing the positions at which pred
   is true for items in coll."
  [pred coll]
  (for [[idx elt] (indexed coll) :when (pred elt)] idx))