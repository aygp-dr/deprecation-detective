(ns deprecation-detective.specs
  "Data specs for deprecation-detective (https://clojure.org/guides/spec).
  Function specs (s/fdef) live next to each defn in deprecation-detective.core."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as str]))

(defn- gen-non-blank [] (gen/not-empty (gen/string-alphanumeric)))

;; --- Shared vocabulary ---

(s/def ::lang #{"python" "javascript" "java" "go" "ruby" "rust" "clojure"})
(s/def ::severity #{"high" "medium" "low"})

;; A relative or absolute file path, as a string or java.nio.file.Path.
(defn- gen-path-string []
  (gen/fmap (fn [[dirs base ext]]
              (str/join "/" (conj dirs (cond-> base ext (str "." ext)))))
            (gen/tuple (gen/vector (gen-non-blank) 0 3)
                       (gen-non-blank)
                       (gen/one-of [(gen/return nil)
                                    (gen/elements ["py" "js" "ts" "go" "java" "rb" "clj" "txt"])
                                    (gen/string-alphanumeric)]))))

(s/def ::path-string (s/with-gen (s/and string? seq) gen-path-string))
(s/def ::path-like
  (s/with-gen (s/or :string ::path-string
                    :path #(instance? java.nio.file.Path %))
    gen-path-string))

;; --- Pattern database (deprecation-detective.core/deprecation-patterns) ---

(s/def ::id (s/with-gen (s/and string? (complement str/blank?)) gen-non-blank))
(s/def ::pattern
  (s/with-gen #(instance? java.util.regex.Pattern %)
    #(gen/fmap re-pattern (gen/elements ["(?i)import\\s+imp\\b" "\\bvar\\s+"
                                         "new\\s+Date\\(\\)" "\"io/ioutil\""]))))
(s/def ::message string?)
(s/def ::replacement string?)

(s/def ::deprecation-pattern
  (s/keys :req-un [::id ::lang ::pattern ::message ::severity ::replacement]))

(s/def ::deprecation-patterns
  (s/and (s/coll-of ::deprecation-pattern :kind vector? :gen-max 5)
         (fn [ps] (or (empty? ps) (apply distinct? (map :id ps))))))

;; --- Findings (one per matching line per pattern) ---

(s/def ::file string?)
(s/def ::line pos-int?)
(s/def ::match string?)
(s/def ::finding
  (s/keys :req-un [::file ::line ::id ::severity ::message ::replacement ::match]))
(s/def ::findings (s/coll-of ::finding :kind sequential? :gen-max 10))

;; --- CLI (babashka.cli result; --severity is the minimum severity) ---

(s/def ::dir ::path-string)
(s/def ::format #{"text" "json" "edn"})
(s/def ::help boolean?)
(s/def ::cli-opts (s/keys :opt-un [::dir ::format ::severity ::help]))
