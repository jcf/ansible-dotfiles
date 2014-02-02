{:user {:signing {:gpg-key "8ED1CE42"}

        :dependencies [[clj-stacktrace "0.2.7"]
                       [org.clojure/tools.namespace "0.2.4"]
                       [slamhound "1.3.1"]
                       [spyscope "0.1.4"]
                       [criterium "0.4.2"]]

        :plugins [[codox "0.6.6"]
                  [jonase/eastwood "0.0.2"]
                  [lein-cljsbuild "1.0.0"]
                  [lein-clojars "0.9.1"]
                  [lein-cloverage "1.0.2"]
                  [lein-difftest "2.0.0"]
                  [lein-kibit "0.0.8"]
                  [lein-marginalia "0.7.1"]
                  [lein-pprint "1.1.1"]
                  [lein-swank "1.4.4"]]

        :injections [(require 'spyscope.core
                              '[clojure.repl :refer [source]]
                              '[clojure.tools.namespace.repl :refer [refresh]]
                              '[criterium.core :refer [bench quick-bench]])]

        :aliases {"slamhound" ["run" "-m" "slam.hound"]}
        :search-page-size 30}}
