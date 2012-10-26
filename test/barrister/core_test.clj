(ns barrister.core-test
  (:require [cheshire.core])
  (:require [criterium.core])
  (:use [barrister.core])
  (:use [clojure.test])
  (:use [slingshot.slingshot :only [throw+]]))

(def c (barrister.core/load-contract (slurp "test/conform.json")))
(def h { "A.add" (fn [a b] (+ a b)) 
         "A.calc" (fn [nums op]
                    (throw+ {:type :rpc-err :code 30202 :message "blah.."})) })

(deftest bench-dispatch
  (is (= 65 (:result (dispatch-req c h (create-rpc-req "A.add" [32 33])))))
  (criterium.core/quick-bench (dispatch-req c h (create-rpc-req "A.add" [32 33]))))

;;(deftest can-load-contract
;;  (let [c (cheshire.core/parse-string (slurp "test/conform.json"))]
;;    (is (= "34f6238ed03c6319017382e0fdc638a7" (:checksum (:meta c))))))
