(ns deprecation-detective.specs-test
  "Generative checks for every pure s/fdef'd fn, plus data-spec sanity.
  Per https://clojure.org/guides/spec (Testing)."
  (:require [babashka.fs :as fs]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest is testing]]
            [deprecation-detective.core :as sut]
            [deprecation-detective.specs :as specs]))

(def ^:private check-opts {:clojure.spec.test.check/opts {:num-tests 50}})

;; Side-effecting fns: fdef'd for instrumentation, never generatively checked.
;; scan-file/scan-directory read the filesystem; -main prints and exits.
(def ^:private side-effecting
  #{`sut/scan-file `sut/scan-directory `sut/-main})

(defn- checkable []
  (remove side-effecting (stest/enumerate-namespace 'deprecation-detective.core)))

(deftest fdefs-hold-under-generative-testing
  (let [results (stest/check (checkable) check-opts)]
    (is (seq results) "expected at least one fdef'd fn to check")
    (doseq [r results]
      (testing (str (:sym r))
        (is (nil? (:failure r))
            (pr-str (stest/abbrev-result r)))))))

(deftest data-specs-generate-and-conform
  (doseq [k [::specs/path-like ::specs/deprecation-pattern ::specs/deprecation-patterns
             ::specs/finding ::specs/findings ::specs/cli-opts]]
    (testing (str k)
      (is (every? (fn [[v _]] (s/valid? k v)) (s/exercise k 10))))))

(deftest real-values-conform
  (testing "pattern database and lookup tables"
    (is (s/valid? ::specs/deprecation-patterns sut/deprecation-patterns))
    (is (s/valid? (s/map-of string? ::specs/lang) sut/ext->lang)))
  (testing "a real scan result"
    (let [tmp (fs/create-temp-file {:prefix "dd-spec-" :suffix ".py"})]
      (try
        (spit (str tmp) "from distutils.core import setup\nimport optparse\n")
        (is (s/valid? ::specs/findings (sut/scan-file tmp)))
        (finally (fs/delete tmp)))))
  (testing "the babashka.cli defaults"
    (is (s/valid? ::specs/cli-opts {:dir "." :format "text" :severity "low"}))))
