{:user {:signing {:gpg-key "8ED1CE42"}

        :dependencies [[alembic "0.2.1"]
                       [clj-stacktrace "0.2.7"]
                       [criterium "0.4.2"]
                       [org.clojure/tools.namespace "0.2.5"]
                       [org.clojure/tools.nrepl "0.2.7"]
                       [slamhound "1.5.3"]
                       [spyscope "0.1.5"]]

        :plugins [[cider/cider-nrepl "0.9.0-SNAPSHOT"]
                  [codox "0.6.6"]
                  [jonase/eastwood "0.1.4"]
                  [lein-clojars "0.9.1"]
                  [lein-cloverage "1.0.2"]
                  [lein-difftest "2.0.0"]
                  [lein-kibit "0.0.8"]
                  [lein-marginalia "0.7.1"]
                  [lein-pprint "1.1.1"]
                  [com.palletops/lein-shorthand "0.4.0"]
                  [lein-swank "1.4.4"]
                  [lein-try "0.4.3"]
                  [lein-typed "0.3.5"]
                  [refactor-nrepl "0.2.2"]]

        :injections [(require 'spyscope.core)]

        :shorthand {. [^:lazy alembic.still/distill
                       ^:lazy alembic.still/load-project
                       ^:lazy ^:macro alembic.still/lein
                       ^:lazy clojure.java.shell/sh
                       ^:lazy clojure.pprint/pprint
                       clojure.repl/apropos
                       clojure.repl/dir
                       clojure.repl/doc
                       clojure.repl/find-doc
                       clojure.repl/pst
                       clojure.repl/source
                       ^:lazy clojure.test/run-all-tests
                       ^:lazy clojure.test/run-tests
                       ^:lazy clojure.tools.namespace.repl/refresh
                       ^:lazy clojure.tools.namespace.repl/refresh-all
                       ^:lazy ^:macro criterium.core/bench
                       ^:lazy ^:macro criterium.core/quick-bench]}

        :aliases {"slamhound" ["run" "-m" "slam.hound"]}
        :search-page-size 50}}
