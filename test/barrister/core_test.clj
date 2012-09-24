(ns barrister.core
  (:require [cheshire.core])
  (:use [barrister.core])
  (:use [clojure.test]))

(deftest can-load-contract
  (let [c (cheshire.core/parse-string (slurp "test/conform.json"))]
    (is (= "34f6238ed03c6319017382e0fdc638a7" (:checksum (:meta c))))))
