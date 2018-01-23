(defproject simple-failure "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [compojure "1.6.0"]
                 [ring/ring-defaults "0.3.1"]
                 [ring/ring-json "0.4.0"]
                 [ring/ring-mock "0.3.2"]
                 [failjure "1.2.0"]]
  :plugins [[lein-ring "0.12.3"]]
  :ring {:handler simple-failure.handler/app
         :stacktraces? false}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]]}})
