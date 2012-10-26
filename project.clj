(defproject barrister "0.0.1"
  :description "Barrister RPC bindings for Clojure"
  :profiles {:dev
             {:dependencies
              [[lein-marginalia "0.7.1"]
               [lein-autodoc "0.9.0"]
               [criterium "0.3.0"]]}}
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/tools.logging "0.2.3"]
                 [org.clojure/tools.nrepl "0.2.0-beta10"]
                 [clj-http "0.5.5"]
                 [cheshire "4.0.3"]
                 [slingshot "0.10.3"]])

