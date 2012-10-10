(ns barrister.client
  (:require [cheshire.core])
  (:require [barrister.core :as barrister])
  (:require [clj-http.client :as http]))

(defn handle-resp [resp]
  (cond
    (not (nil? (:error resp))) (barrister/throw-err (:error resp))
    (not (nil? (:result resp))) (:result resp)
    :else (barrister/throw-err -32000 (str "Invalid JSON-RPC response: " (cheshire.core/generate-string resp)))))

(defn call [method params contract trans]
  (let [ req        (barrister/create-rpc-req method params)
         valid-resp (barrister/validate-rpc-req contract req)
         rpc-resp   (if (nil? valid-resp) (trans req) valid-resp) ]
    (handle-resp rpc-resp)))

(defn call-load-contract [trans]
  (let [ req      (barrister/create-rpc-req "barrister-idl" [ ])
         rpc-resp (trans req) ]
  (handle-resp rpc-resp)))

(defn http-req 
  ([method params url opts] (http-req (barrister/create-rpc-req method params) url opts))
  ([req url opts]
     (let [ body (cheshire.core/generate-string req {:escape-non-ascii true}) 
            resp (http/post url (assoc opts :body body)) ]
       (cheshire.core/parse-string (:body resp) true))))

(defn make-http-trans 
  ([url] (make-http-trans url { }))
  ([url opts]
     (fn [req]
       (http-req req url opts))))