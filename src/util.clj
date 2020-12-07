(ns util
  (:require [clojure.java.io :as io]))

(defn input-lines
  "Parse a resource"
  [file-name]
  (->> file-name
       io/resource
       io/reader
       line-seq))
