#!/usr/bin/env janet
# scmeta - Janet variant
# Janet has built-in JSON support via spork or json module

(def VERSION "1.0.0")
(def PI 3.141592653589793)

(defn usage []
  (print "Usage: scmeta.janet [arguments]")
  (print "")
  (print "Options:")
  (print "  --lang-name=NAME        Language name")
  (print "  --target-name=TARGET    Earthfile target name")
  (print "  --lang-version=CMD      Command to get version")
  (print "  --lang-version-match-index=N  Version match index (default 0)")
  (print "  --hyperfine=FILE        Path to hyperfine JSON")
  (print "  --pi=FILE               Path to pi.txt")
  (print "  --output=FILE           Output JSON path")
  (print "  -h, --help              Show this help")
  (print "  -v, --version           Show version"))

# Calculate pi accuracy: -log10(|1 - (value / PI)|)
(defn pi-accuracy [value]
  (let [ratio (/ value PI)
        diff (math/abs (- 1 ratio))]
    (if (= diff 0)
      999
      (- (/ (math/log diff) (math/log 10))))))

# Extract version from text at given index
(defn get-version [text match-index]
  (def pattern (peg/compile ~(capture (sequence (some :d) (some (sequence "." (some :d)))))))
  (def versions (peg/match pattern text))
  (if (and versions (> (length versions) match-index))
    (get versions match-index)
    (error (string "No version found at index " match-index))))

(defn to-timedelta [num]
  (string num "s"))

# Run shell command and capture output
(defn run-cmd [cmd]
  (let [proc (os/spawn ["sh" "-c" cmd] :p {:out :pipe :err :pipe})
        out (ev/read (proc :out) :all)
        err (ev/read (proc :err) :all)]
    (os/proc-wait proc)
    (string out err)))

# Simple JSON encoder (Janet's json module may need spork)
(defn json-encode [obj &opt indent]
  (default indent 0)
  (def spaces (string/repeat "  " indent))
  (cond
    (dictionary? obj)
    (string "{\n"
            (string/join
              (map (fn [[k v]]
                     (string spaces "  \"" k "\": " (json-encode v (+ indent 1))))
                   (pairs obj))
              ",\n")
            "\n" spaces "}")

    (indexed? obj)
    (string "[" (string/join (map |(json-encode $) obj) ", ") "]")

    (string? obj)
    (string "\"" obj "\"")

    (number? obj)
    (string obj)

    (boolean? obj)
    (if obj "true" "false")

    (nil? obj)
    "null"

    (string obj)))

# Simple JSON decoder using PEG
(def json-grammar
  ~{:main (choice :object :array :string :number :true :false :null)
    :ws (any (set " \t\n\r"))
    :object (cmt (sequence "{" :ws (any (sequence :pair :ws (any (sequence "," :ws :pair :ws)))) "}")
                 ,|(struct ;(flatten $&)))
    :pair (sequence :string :ws ":" :ws :main)
    :array (cmt (sequence "[" :ws (any (sequence :main :ws (any (sequence "," :ws :main :ws)))) "]")
                ,|(array ;$&))
    :string (cmt (sequence "\"" (capture (any (choice (sequence "\\" 1) (if-not "\"" 1)))) "\"")
                 ,|(string $))
    :number (cmt (capture (sequence (opt "-") (some :d) (opt (sequence "." (some :d)))
                                    (opt (sequence (set "eE") (opt (set "+-")) (some :d)))))
                 ,|(scan-number $))
    :true (cmt "true" ,|(identity true))
    :false (cmt "false" ,|(identity false))
    :null (cmt "null" ,|(identity nil))})

(defn json-decode [text]
  (first (peg/match json-grammar text)))

# Parse command line arguments
(var lang-name nil)
(var target-name nil)
(var lang-version-cmd nil)
(var lang-version-match-index 0)
(var hyperfine-file nil)
(var pi-file nil)
(var output-file nil)

(each arg (dyn :args)
  (cond
    (or (= arg "-h") (= arg "--help"))
    (do (usage) (os/exit 0))

    (or (= arg "-v") (= arg "--version"))
    (do (print "scmeta " VERSION) (os/exit 0))

    (string/has-prefix? "--lang-name=" arg)
    (set lang-name (string/slice arg (length "--lang-name=")))

    (string/has-prefix? "--target-name=" arg)
    (set target-name (string/slice arg (length "--target-name=")))

    (string/has-prefix? "--lang-version=" arg)
    (set lang-version-cmd (string/slice arg (length "--lang-version=")))

    (string/has-prefix? "--lang-version-match-index=" arg)
    (set lang-version-match-index (scan-number (string/slice arg (length "--lang-version-match-index="))))

    (string/has-prefix? "--hyperfine=" arg)
    (set hyperfine-file (string/slice arg (length "--hyperfine=")))

    (string/has-prefix? "--pi=" arg)
    (set pi-file (string/slice arg (length "--pi=")))

    (string/has-prefix? "--output=" arg)
    (set output-file (string/slice arg (length "--output=")))))

# Validate required arguments
(unless lang-name (eprint "ERROR: --lang-name is required!") (os/exit 1))
(unless target-name (eprint "ERROR: --target-name is required!") (os/exit 1))
(unless hyperfine-file (eprint "ERROR: --hyperfine is required!") (os/exit 1))
(unless pi-file (eprint "ERROR: --pi is required!") (os/exit 1))
(unless output-file (eprint "ERROR: --output is required!") (os/exit 1))
(unless lang-version-cmd (eprint "ERROR: --lang-version is required!") (os/exit 1))

# Read pi value and calculate accuracy
(def computed-pi (string/trim (slurp pi-file)))
(when (empty? computed-pi)
  (eprint "ERROR: Pi file is empty!")
  (os/exit 1))
(def accuracy (pi-accuracy (scan-number computed-pi)))

# Get language version
(def version-output (run-cmd lang-version-cmd))
(def lang-version (get-version version-output lang-version-match-index))

# Parse hyperfine JSON
(def hyperfine (json-decode (slurp hyperfine-file)))
(def result (get-in hyperfine [:results 0]))

# Build output
(def metadata
  {"Language" lang-name
   "Target" target-name
   "Version" lang-version
   "Command" (get result :command)
   "CalculatedPi" computed-pi
   "Accuracy" accuracy
   "Mean" (to-timedelta (get result :mean))
   "Stddev" (to-timedelta (get result :stddev))
   "UserTime" (to-timedelta (get result :user))
   "SystemTime" (to-timedelta (get result :system))
   "Median" (to-timedelta (get result :median))
   "Min" (to-timedelta (get result :min))
   "Max" (to-timedelta (get result :max))
   "TimesPerRun" (get result :times)
   "ExitCodesPerRun" (get result :exit_codes)})

# Write output
(spit output-file (json-encode metadata))

(print "Successfully created metadata")
(print (string "Language: " lang-name " (" lang-version ")"))
(print (string "Output: " output-file))
