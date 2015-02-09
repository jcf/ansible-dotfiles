{:user {:signing {:gpg-key "8ED1CE42"}

        :dependencies [[alembic "0.2.1"]
                       [clj-stacktrace "0.2.7"]
                       [cljfmt "0.1.7"]
                       [criterium "0.4.2"]
                       [org.clojure/tools.namespace "0.2.5"]
                       [rewrite-clj "0.4.11"
                        :exclusions [org.clojure/tools.reader]]
                       [slamhound "1.5.3"]
                       [spyscope "0.1.4"]]

        :plugins [#_[cider/cider-nrepl "0.9.0-SNAPSHOT"]
                  [codox "0.6.6"]
                  [jonase/eastwood "0.1.4"]
                  [lein-ancient "0.5.5" :exclusions [commons-codec]]
                  [lein-cljfmt "0.1.7" :exclusions [rewrite-clj]]
                  [lein-cljsbuild "1.0.3"]
                  [lein-clojars "0.9.1"]
                  [lein-cloverage "1.0.2"]
                  [lein-difftest "2.0.0"]
                  [lein-kibit "0.0.8"]
                  [lein-marginalia "0.7.1"]
                  [lein-pprint "1.1.1"]
                  [lein-swank "1.4.4"]
                  [lein-try "0.4.3"]
                  [lein-typed "0.3.5"]
                  [refactor-nrepl "0.2.2"]]

        :injections [(require
                      '[alembic.still :refer [distill]]
                      '[clojure.repl :refer [doc source]]
                      '[clojure.tools.namespace.repl :as repl]
                      '[criterium.core :refer [bench quick-bench]]
                      'spyscope.core)]

        :aliases {"slamhound" ["run" "-m" "slam.hound"]}
        :search-page-size 50}}
