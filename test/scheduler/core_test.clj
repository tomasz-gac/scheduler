(ns scheduler.core-test
  (:require [clojure.test :refer :all]
            [scheduler.core :refer :all]
            [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]))

(deftest work-has-nonzero-duration
  (testing
    (is (= '()
           (l/run* [q p] (fd/in q p (fd/interval 0 100)) (worko q 0 p))))))
