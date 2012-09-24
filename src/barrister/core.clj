(ns barrister.core
  (:require [clojure.string]
            [clojure.tools.logging :as log])
  (:use [slingshot.slingshot :only [try+]]))

;;(defmacro dbg[x] `(let [x# ~x] (println '~x "=" x#) x#))

;; (defn load-contract [json]
;;   (cheshire.core/parse-string json))

(declare validate-field)

(defn first-not-nil [s]
  (first (filter #(not (nil? %)) s)))

(defn eval-until-not-nil [fn-seq]
  (loop [f (first fn-seq)
         remain (rest fn-seq)]
    (let [out (if-not (nil? f) (f))]
      (if (nil? out)
        (if (empty? remain)
          nil
          (recur (first remain) (rest remain)))
        out))))

(defn all-elem [c type]
  (filter #(= type (get % "type")) c))

(defn all-interfaces [c]
  (all-elem c "interface"))
  
(defn get-elem [c name type]
  (first (filter #(and (= type (get % "type")) (= name (get % "name"))) c)))

(defn get-struct [c name]
  (get-elem c name "struct"))

(defn get-enum [c name]
  (get-elem c name "enum"))

(defn get-enum-vals [c name]
  (map #(get % "value") (get (get-enum c name) "values")))

(defn get-interface [c name]
  (get-elem c name "interface"))

(defn parse-method [m]
  (clojure.string/split m #"\."))

(defn get-function [c iface func]
  (first (filter #(= func (get % "name")) 
                 (get (get-interface c iface) "functions"))))

(defn get-meth [c name]
  (let [m (parse-method name)]
    (get-function c (first m) (second m))))

(defn all-functions [c]
  (let [ifaces (all-elem c "interface")]
    (reduce conj (map #(get % "functions") ifaces))))

(defn is-bool? [val]
  (instance? Boolean val))

(defn is-int? [val]
  (or (instance? Long val) (instance? Integer val)))

(defn is-float? [val]
  (or (instance? Double val) (instance? Float val) (is-int? val)))

(defn struct-field-map [c struct]
  (let [f (get struct "fields")
        parent (get-struct c (get struct "extends"))
        parent-map (if (nil? parent) {} (struct-field-map c parent))]
    (if (nil? f)
      {}
      (apply assoc (cons parent-map (interleave (map #(get % "name") f) f))))))

(defn validate-prim [msg valid expect-type val]
  (if-not valid (str msg "is type '" (type val) "' not " expect-type)))

(defn validate-struct [msg c struct val]
  (let [f (struct-field-map c struct)]
    (if (map? val)
      (first-not-nil (map #(validate-field msg c (get f %) (get val %)) (keys f)))
      (str msg "'" val "' is type '" (type val) "' not " (get struct "name")))))

(defn validate-enum [msg c elem val]
  (let [vals (set (map #(get % "value") (get elem "values")))
        name (get elem "name")]
    (if-not (contains? vals val)
      (str msg "'" val "' is not in enum " name " (valid values: " (clojure.string/join ", " vals) ")"))))

(defn validate-cust-type [msg c expect-type val]
  (let [elem (eval-until-not-nil [#(get-struct c expect-type) #(get-enum c expect-type)])]
    (case (get elem "type")
      "struct" (validate-struct msg c elem val)
      "enum"   (validate-enum msg c elem val)
      (str msg "unknown type: " expect-type))))

(defn validate-type [msg c expect-type optional? val]
  (if (nil? val)
    ;; val is nil
    (if-not optional?
      (str msg "cannot be null"))
    ;; val is not nil
    (case expect-type
      "string" (validate-prim msg (string? val)   expect-type val)
      "int"    (validate-prim msg (is-int? val)   expect-type val)
      "float"  (validate-prim msg (is-float? val) expect-type val)
      "bool"   (validate-prim msg (is-bool? val)  expect-type val)
      (validate-cust-type msg c expect-type val)
      )))

(defn validate-field [msg c field val]
  (let [type (get field "type")
        name (get field "name")
        optional? (get field "optional")
        expect-arr (get field "is_array")
        msg (str msg name " ")]
    (if (not= (sequential? val) expect-arr)
      ;; array type mismatch
      (if expect-arr
        (str msg "'" name "' expects an array")
        (str msg "'" name "' expects type '" type "' not an array"))
      ;; array type match
      (if expect-arr
        (first-not-nil (map #(validate-type msg c type optional? %) val))
        (validate-type msg c type optional? val)))))

(defn validate-params [c func params]
  (let [f-params (get func "params")
        f-name   (get func "name")]
    (if (nil? func)
      (str "Function is undefined")
      (if (= (count f-params) (count params))
        (first-not-nil (map #(validate-field (str "Function '" f-name "' param ") c %1 %2) f-params params))
        (str "Function '" f-name "' expects " (count f-params) " param(s) but received " (count params))))))

(defn create-rpc-resp
  ([req result]
     { "id" (get req "id") "result" result })
  ([req code msg] 
     (if-not (nil? msg)
       { "id" (get req "id") "error" { "code" code "message" msg } }))
  ([req code msg data]
     (let [err (create-rpc-resp req code msg)]
       (if-not (nil? err) (assoc-in err ["error" "data"] data)))))

(defn create-rpc-req [method params]
  { "id" (.toString (java.util.UUID/randomUUID)) "method" method "params" params })

;; from: http://stackoverflow.com/questions/1696693/clojure-how-to-find-out-the-arity-of-function-at-runtime
(defn get-arity [f]
  (let [m (first (.getDeclaredMethods (class f)))
        p (.getParameterTypes m)]
    (alength p)))

(defn validate-handler [c iface func handlers]
  (let [method (str (get iface "name") "." (get func "name"))
        h (get handlers method)
        param-count (count (get func "params"))
        h-arity (if-not (nil? h) (get-arity h))]
    (cond 
      (nil? h) (str "No handler defined for method: " method)
      (not= param-count h-arity) (str "Method: " method " expects " param-count " param(s) but handler has " h-arity)
      :else nil)))

(defn validate-interface-handlers [c iface handlers]
  (filter string? (map #(validate-handler c iface % handlers) (get iface "functions"))))

(defn validate-handlers [c handlers]
  (flatten (map #(validate-interface-handlers c % handlers) (all-interfaces c))))

(defn validate-result [c func req result]
  (let [name (str (get req "method") " return value")
        err  (validate-field name c (get func "returns") result)]
    (if (nil? err)
      (create-rpc-resp req result)
      (create-rpc-resp req -32001 err))))

(defn invoke-one [c handlers req]
  (let [method  (get req "method")
        func    (if-not (nil? method) (get-meth c method))
        params  (if-not (nil? func)   (get req "params"))
        handler (if-not (nil? method) (get handlers method))]
    (cond
      (nil? method)  (create-rpc-resp req -32600 "req missing 'method' property")
      (nil? func)    (create-rpc-resp req -32601 (str "Unknown method: " method))
      (nil? handler) (create-rpc-resp req -32601 (str "No handler for method: " method))
      :else (eval-until-not-nil
             [ #(create-rpc-resp req -32602 (validate-params c func params))
               #(try+
                  (validate-result c func req (apply handler params))
                  (catch [:type :rpc-err] {:keys [code message data]} (create-rpc-resp req code message data))
                  (catch Object _
                    (let [msg (str "Unexpected error executing: " method)]
                      (log/error (:throwable &throw-context) msg)
                      (create-rpc-resp req -32000 msg)))) ]))))

(defn invoke [c handlers req]
  (if (sequential? req)
    (map #(invoke-one c handlers %) req)
    (invoke-one c handlers req)))
