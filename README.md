# barrister-clj

FIXME: write description

## Usage

FIXME: write

## Examples

    Load contract:
    user> (def c (cheshire.core/parse-string (slurp "test/conform.json")))
    
    Define handlers:
    user> (use '[slingshot.slingshot :only [throw+]])
    user> (def h { "A.add" (fn [a b] (+ a b)) 
                   "A.calc" (fn [nums op]
                              (throw+ {:type :rpc-err :code 30202 :message "blah.."})) })

    Benchmarking:
    user> (require 'criterium.core)
    user> (criterium.core/quick-bench (invoke c h (create-rpc-req "A.add" [32 33])))
    

## License

Copyright (C) 2012 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
