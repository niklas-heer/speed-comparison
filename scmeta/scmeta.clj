#!/usr/bin/env bb -o

(require '[docopt.core :as docopt]
         '[babashka.process :refer [sh]]
         '[cheshire.core :as json]
         '[clojure.string :as str])

(defn wrap-as-time
  "Takes a given `time` in seconds and returns it as a time String.\n
   This is useful for working with dataframes. (appends an s)"
  [time]
  (str time "s"))

(defn parse-int
  "Takes a given String [s] and returns an Integer."
  [s]
  (Integer/parseInt (re-find #"\A-?\d+" s)))

(defn read-hf-file
  "Takes a hyperfine.json result file and returns a flattened hash-map."
  [file]
  (->  file
       (slurp)
       (json/parse-string true)
       :results
       first))

(defn read-pi-file
  "Takes a pi.txt file and returns the content as a trimmed String."
  [file]
  (-> file
      (slurp)
      (str/trim-newline)))

(defn pi-accuracy
  ;; https://github.com/niklas-heer/speed-comparison/issues/78#issuecomment-1292708468
  "Takes a given number for Pi and returns the accuracy.

   The higher the number the better:
     - pi-accuracy(3.14) = 3.29
     - pi-accuracy(3.145) = 2.96
     - pi-accuracy(3.1415) = 4.53
     - pi-accuracy(3.1415926535897) = 13.5"
  [pi]
  (let [x (Double/parseDouble pi)]
    ;; Formular: accuracy(x) = -log10(abs(1-x/pi))
    (- (Math/log10 (abs (- 1 (/ x Math/PI)))))))

(defn get-version
  "Takes a command as `cmd` which gets executed as a shell process.
   The command should return some version number like: `1.0.0`

   The second parameter `index` determines which version number to return, should there be multiple.

   Returns a version number as String."
  [cmd index]
  (-> cmd
      (sh)
      :out
      ;; re-seq needs the input as the last argument so we use "thread-last" (->>)
      (->> (re-seq #"\d+(\.\d+)+"))
      (nth index)
      first))

(defn generate-json [arg-map]
  (let [lvi       (parse-int    (arg-map "--lvi"))
        lv        (get-version  (arg-map "--lv") lvi)
        pi        (read-pi-file (arg-map "--pi"))
        hyperfine (read-hf-file (arg-map "--hf"))
        accuracy  (pi-accuracy pi)]
    (json/generate-string {:Language         (arg-map "--ln")
                           :Version          lv
                           :Command          (hyperfine :command)
                           :CalculatedPi     pi
                           :Accuracy         accuracy
                           :Mean             (wrap-as-time (hyperfine :mean))
                           :Median           (wrap-as-time (hyperfine :median))
                           :Min              (wrap-as-time (hyperfine :min))
                           :Max              (wrap-as-time (hyperfine :max))
                           :Stddev           (wrap-as-time (hyperfine :stddev))
                           :UserTime         (wrap-as-time (hyperfine :user))
                           :SystemTime       (wrap-as-time (hyperfine :system))
                           :TimesPerRun      (hyperfine :times)
                           :ExitCodesPerRun  (hyperfine :exit_codes)}
                          {:pretty true})))

(def usage "SCMeta.

Usage:
  scmeta [options]

Options:
  --ln=<name>   Name of the language.
  --lv=<cmd>    Command to get the language version.
  --lvi=<int>   If there are multiple versions for `lv`, select the index [default: 0].
  --hf=<json>   Hyperfine JSON file path.
  --pi=<txt>    Path to pi.txt file.
  --out=<json>  Path to write the output to")

(docopt/docopt usage
               *command-line-args*
               (fn [arg-map]
                 (->> arg-map
                      (generate-json)
                      (spit (arg-map "--out")))))
