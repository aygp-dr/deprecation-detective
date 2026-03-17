(ns deprecation-detective.core-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [deprecation-detective.core :as sut]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [cheshire.core :as json]))

;; ---------------------------------------------------------------------------
;; Pattern positive-match tests
;; ---------------------------------------------------------------------------

(deftest pattern-positive-matches
  (testing "Each pattern regex matches its intended target string"
    (let [patterns sut/deprecation-patterns
          positives {"py-distutils"        "from distutils.core import setup"
                     "py-imp"              "import imp"
                     "py-optparse"         "import optparse"
                     "py-cgi"              "import cgi"
                     "py-unittest-makesuite" "suite = unittest.makeSuite(MyTest)"
                     "js-var"              "var x = 42;"
                     "js-arguments"        "console.log(arguments[0])"
                     "node-domain"         "const d = require('domain')"
                     "java-date"           "Date d = new Date();"
                     "go-ioutil"           "\"io/ioutil\""}]
      (doseq [{:keys [id pattern]} patterns]
        (let [input (get positives id)]
          (is (some? input) (str "Missing positive test case for pattern " id))
          (is (re-find pattern input)
              (str "Pattern " id " should match: " (pr-str input))))))))

;; ---------------------------------------------------------------------------
;; Pattern negative-match tests (no false positives)
;; ---------------------------------------------------------------------------

(deftest pattern-negative-matches
  (testing "Patterns do not match innocent strings"
    (let [negatives {"py-distutils"        "import setuptools"
                     "py-imp"              "import importlib"
                     "py-optparse"         "import argparse"
                     "py-cgi"              "import cgitb_wrapper"
                     "py-unittest-makesuite" "unittest.TestLoader()"
                     "js-var"              "let x = 42;"
                     "js-arguments"        "const args = [...rest]"
                     "node-domain"         "require('express')"
                     "java-date"           "Instant.now()"
                     "go-ioutil"           "\"io\""}
          patterns sut/deprecation-patterns]
      (doseq [{:keys [id pattern]} patterns]
        (let [input (get negatives id)]
          (is (some? input) (str "Missing negative test case for pattern " id))
          (is (nil? (re-find pattern input))
              (str "Pattern " id " should NOT match: " (pr-str input))))))))

;; ---------------------------------------------------------------------------
;; file-extension helper
;; ---------------------------------------------------------------------------

(deftest file-extension-test
  (testing "Extracts file extensions correctly"
    (is (= "py"  (sut/file-extension "foo.py")))
    (is (= "js"  (sut/file-extension "bar.js")))
    (is (= "go"  (sut/file-extension "main.go")))
    (is (= "clj" (sut/file-extension "core.clj")))
    (is (nil?    (sut/file-extension "Makefile")))))

;; ---------------------------------------------------------------------------
;; ext->lang mapping
;; ---------------------------------------------------------------------------

(deftest ext-lang-mapping-test
  (testing "Extension-to-language mapping covers expected extensions"
    (is (= "python"     (sut/ext->lang "py")))
    (is (= "python"     (sut/ext->lang "pyw")))
    (is (= "javascript" (sut/ext->lang "js")))
    (is (= "javascript" (sut/ext->lang "ts")))
    (is (= "java"       (sut/ext->lang "java")))
    (is (= "go"         (sut/ext->lang "go")))
    (is (= "clojure"   (sut/ext->lang "clj")))
    (is (nil?           (sut/ext->lang "md")))))

;; ---------------------------------------------------------------------------
;; scan-file with temp files
;; ---------------------------------------------------------------------------

(deftest scan-file-python-test
  (testing "scan-file detects deprecations in a Python file"
    (let [tmp (fs/create-temp-file {:prefix "dd-test-" :suffix ".py"})]
      (try
        (spit (str tmp) "from distutils.core import setup\nimport imp\nimport os\n")
        (let [results (sut/scan-file tmp)]
          (is (= 2 (count results)))
          (is (some #(= "py-distutils" (:id %)) results))
          (is (some #(= "py-imp" (:id %)) results))
          (is (every? #(= (str tmp) (:file %)) results)))
        (finally
          (fs/delete tmp))))))

(deftest scan-file-javascript-test
  (testing "scan-file detects deprecations in a JavaScript file"
    (let [tmp (fs/create-temp-file {:prefix "dd-test-" :suffix ".js"})]
      (try
        (spit (str tmp) "var x = 1;\nconst y = arguments[0];\nlet z = 2;\n")
        (let [results (sut/scan-file tmp)]
          (is (= 2 (count results)))
          (is (some #(= "js-var" (:id %)) results))
          (is (some #(= "js-arguments" (:id %)) results)))
        (finally
          (fs/delete tmp))))))

(deftest scan-file-go-test
  (testing "scan-file detects deprecations in a Go file"
    (let [tmp (fs/create-temp-file {:prefix "dd-test-" :suffix ".go"})]
      (try
        (spit (str tmp) "package main\n\nimport \"io/ioutil\"\n\nfunc main() {}\n")
        (let [results (sut/scan-file tmp)]
          (is (= 1 (count results)))
          (is (= "go-ioutil" (:id (first results)))))
        (finally
          (fs/delete tmp))))))

(deftest scan-file-java-test
  (testing "scan-file detects deprecations in a Java file"
    (let [tmp (fs/create-temp-file {:prefix "dd-test-" :suffix ".java"})]
      (try
        (spit (str tmp) "import java.util.Date;\npublic class Foo { Date d = new Date(); }\n")
        (let [results (sut/scan-file tmp)]
          (is (= 1 (count results)))
          (is (= "java-date" (:id (first results)))))
        (finally
          (fs/delete tmp))))))

(deftest scan-file-clean-test
  (testing "scan-file returns empty for clean files"
    (let [tmp (fs/create-temp-file {:prefix "dd-test-" :suffix ".py"})]
      (try
        (spit (str tmp) "import os\nimport sys\nprint('hello')\n")
        (let [results (sut/scan-file tmp)]
          (is (empty? results)))
        (finally
          (fs/delete tmp))))))

(deftest scan-file-unknown-extension-test
  (testing "scan-file returns nil for unsupported file types"
    (let [tmp (fs/create-temp-file {:prefix "dd-test-" :suffix ".txt"})]
      (try
        (spit (str tmp) "from distutils import something\n")
        (let [results (sut/scan-file tmp)]
          (is (nil? results)))
        (finally
          (fs/delete tmp))))))

;; ---------------------------------------------------------------------------
;; scan-directory with temp directory
;; ---------------------------------------------------------------------------

(deftest scan-directory-test
  (testing "scan-directory finds deprecations across multiple files"
    (let [tmpdir (fs/create-temp-dir {:prefix "dd-scan-"})
          py-file (str tmpdir "/legacy.py")
          js-file (str tmpdir "/old.js")
          clean-file (str tmpdir "/clean.py")]
      (try
        (spit py-file "from distutils.core import setup\nimport optparse\n")
        (spit js-file "var x = 1;\n")
        (spit clean-file "import os\n")
        (let [results (sut/scan-directory (str tmpdir) {})]
          (is (= 3 (count results)))
          (is (some #(= "py-distutils" (:id %)) results))
          (is (some #(= "py-optparse" (:id %)) results))
          (is (some #(= "js-var" (:id %)) results)))
        (finally
          (fs/delete-tree tmpdir))))))

(deftest scan-directory-empty-test
  (testing "scan-directory returns empty for clean directory"
    (let [tmpdir (fs/create-temp-dir {:prefix "dd-clean-"})
          py-file (str tmpdir "/modern.py")]
      (try
        (spit py-file "import os\nimport sys\n")
        (let [results (sut/scan-directory (str tmpdir) {})]
          (is (empty? results)))
        (finally
          (fs/delete-tree tmpdir))))))

;; ---------------------------------------------------------------------------
;; Output formatting
;; ---------------------------------------------------------------------------

(deftest format-text-empty-test
  (testing "format-text with no findings"
    (is (= "No deprecations found." (sut/format-text [])))))

(deftest format-text-with-findings-test
  (testing "format-text includes file, line, severity, message"
    (let [findings [{:file "test.py" :line 1 :severity "high"
                     :message "distutils deprecated" :replacement "setuptools"
                     :match "from distutils import setup" :id "py-distutils"}]
          output (sut/format-text findings)]
      (is (str/includes? output "Found 1 deprecation(s)"))
      (is (str/includes? output "test.py:1"))
      (is (str/includes? output "[HIGH]"))
      (is (str/includes? output "setuptools"))
      (is (str/includes? output "Summary: 1 high, 0 medium, 0 low")))))

(deftest format-json-valid-test
  (testing "format-json produces valid JSON with expected structure"
    (let [findings [{:file "test.py" :line 1 :severity "high"
                     :message "distutils deprecated" :replacement "setuptools"
                     :match "from distutils import setup" :id "py-distutils"}
                    {:file "test.py" :line 2 :severity "medium"
                     :message "optparse deprecated" :replacement "argparse"
                     :match "import optparse" :id "py-optparse"}]
          json-str (sut/format-json findings)
          parsed (json/parse-string json-str true)]
      (is (some? parsed) "JSON should be parseable")
      (is (= 2 (:total parsed)))
      (is (= 1 (get-in parsed [:by-severity :high])))
      (is (= 1 (get-in parsed [:by-severity :medium])))
      (is (= 0 (get-in parsed [:by-severity :low])))
      (is (= 2 (count (:findings parsed)))))))

(deftest format-json-empty-test
  (testing "format-json with no findings produces valid JSON"
    (let [json-str (sut/format-json [])
          parsed (json/parse-string json-str true)]
      (is (= 0 (:total parsed)))
      (is (empty? (:findings parsed))))))

;; ---------------------------------------------------------------------------
;; Severity filtering
;; ---------------------------------------------------------------------------

(deftest severity-filtering-test
  (testing "Severity filtering works correctly"
    (let [tmpdir (fs/create-temp-dir {:prefix "dd-sev-"})
          py-file (str tmpdir "/mixed.py")]
      (try
        ;; distutils = high, optparse = medium, plus we need a low one via js
        (spit py-file "from distutils.core import setup\nimport optparse\n")
        (spit (str tmpdir "/old.js") "var x = 1;\n") ;; low severity
        (let [all-findings (sut/scan-directory (str tmpdir) {})
              severity-rank {"high" 3 "medium" 2 "low" 1}
              high-only (filter #(>= (get severity-rank (:severity %) 0) 3) all-findings)
              medium-up (filter #(>= (get severity-rank (:severity %) 0) 2) all-findings)]
          (is (= 3 (count all-findings)) "All findings with min=low")
          (is (= 1 (count high-only)) "Only high severity")
          (is (= 2 (count medium-up)) "Medium and above"))
        (finally
          (fs/delete-tree tmpdir))))))

;; ---------------------------------------------------------------------------
;; Result structure
;; ---------------------------------------------------------------------------

(deftest finding-structure-test
  (testing "Each finding has the expected keys"
    (let [tmp (fs/create-temp-file {:prefix "dd-test-" :suffix ".py"})]
      (try
        (spit (str tmp) "from distutils.core import setup\n")
        (let [results (sut/scan-file tmp)
              finding (first results)]
          (is (= 1 (count results)))
          (is (string? (:file finding)))
          (is (= 1 (:line finding)))
          (is (= "py-distutils" (:id finding)))
          (is (= "high" (:severity finding)))
          (is (string? (:message finding)))
          (is (= "setuptools" (:replacement finding)))
          (is (string? (:match finding))))
        (finally
          (fs/delete tmp))))))

;; ---------------------------------------------------------------------------
;; Line numbering
;; ---------------------------------------------------------------------------

(deftest line-numbering-test
  (testing "Line numbers are 1-based and correct"
    (let [tmp (fs/create-temp-file {:prefix "dd-test-" :suffix ".py"})]
      (try
        (spit (str tmp) "import os\nimport sys\nfrom distutils import core\nimport json\n")
        (let [results (sut/scan-file tmp)]
          (is (= 1 (count results)))
          (is (= 3 (:line (first results)))))
        (finally
          (fs/delete tmp))))))

;; ---------------------------------------------------------------------------
;; Multiple findings on same pattern
;; ---------------------------------------------------------------------------

(deftest multiple-matches-same-pattern-test
  (testing "Multiple occurrences of the same pattern are all reported"
    (let [tmp (fs/create-temp-file {:prefix "dd-test-" :suffix ".py"})]
      (try
        (spit (str tmp) "from distutils.core import setup\nfrom distutils.command import build\n")
        (let [results (sut/scan-file tmp)]
          (is (= 2 (count results)))
          (is (every? #(= "py-distutils" (:id %)) results))
          (is (= #{1 2} (set (map :line results)))))
        (finally
          (fs/delete tmp))))))

;; ---------------------------------------------------------------------------
;; Pattern count
;; ---------------------------------------------------------------------------

(deftest pattern-count-test
  (testing "Correct number of patterns loaded"
    (is (= 10 (count sut/deprecation-patterns)))))

;; ---------------------------------------------------------------------------
;; Run tests with -main entry point for bb -m
;; ---------------------------------------------------------------------------

(defn -main [& _args]
  (let [result (run-tests 'deprecation-detective.core-test)]
    (when (or (pos? (:fail result)) (pos? (:error result)))
      (System/exit 1))))
