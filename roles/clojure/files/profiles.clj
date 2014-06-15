{:user {:signing {:gpg-key "8ED1CE42"}

        :dependencies [[alembic "0.2.1"]
                       [clj-stacktrace "0.2.7"]
                       [criterium "0.4.2"]
                       [org.clojure/tools.namespace "0.2.4"]
                       [slamhound "1.5.3"]
                       [spyscope "0.1.4"]]

        :plugins [[cider/cider-nrepl "0.7.0-SNAPSHOT"]
                  [codox "0.6.6"]
                  [jonase/eastwood "0.1.1"]
                  [lein-ancient "0.5.4"]
                  [lein-cljsbuild "1.0.0"]
                  [lein-clojars "0.9.1"]
                  [lein-cloverage "1.0.2"]
                  [lein-difftest "2.0.0"]
                  [lein-kibit "0.0.8"]
                  [lein-marginalia "0.7.1"]
                  [lein-pprint "1.1.1"]
                  [lein-swank "1.4.4"]]

        :injections [(require
                      '[alembic.still :refer [distill]]
                      '[clojure.repl :refer [source]]
                      '[clojure.tools.namespace.repl :as repl]
                      '[criterium.core :refer [bench quick-bench]]
                      'spyscope.core)]

        :aliases {"slamhound" ["run" "-m" "slam.hound"]}
        :search-page-size 50}}