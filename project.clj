(defproject icfp2006 "1.0"
  :description "ICFP 2006 (Monroeville) Implementation"
  :url "http://github.com/mgregson/icfp2006"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [prismatic/plumbing "0.3.3"]
                 [jline "0.9.94"]
                 [commons-io/commons-io "2.4"]
                 [gloss "0.2.2"]
                 [org.clojure/tools.logging "0.3.0"]
                 [log4j/log4j "1.2.17"]
                 [clojure-lanterna "0.9.4"]]

  :aot :all
  :min-lein-version "2.0.0"
  :main io.gregson.icfp.monroeville.um
  :profiles
  {:dev
   {:dependencies [[clj-logging-config "1.9.10"]]}})
