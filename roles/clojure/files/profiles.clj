{:user {:signing {:gpg-key "james@logi.cl"}

        :dependencies [#_[acyclic/squiggly-clojure "0.1.2-SNAPSHOT"]
                       [alembic "0.2.1"]
                       [clj-diff "1.0.0-SNAPSHOT"]
                       [clj-stacktrace "0.2.8"]
                       [com.cemerick/pomegranate "0.3.0"]
                       [criterium "0.4.2"]
                       [org.clojure/tools.namespace "0.2.5"]
                       [org.clojure/tools.nrepl "0.2.7"]
                       [pjstadig/humane-test-output "0.7.0"]
                       [slamhound "1.5.3"]]

        :plugins [[cider/cider-nrepl "0.10.0-SNAPSHOT"]
                  [codox "0.6.6"]
                  [jonase/eastwood "0.2.1"]
                  [lein-ancient "0.6.7"]
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
                  [refactor-nrepl "1.2.0-SNAPSHOT"]]

        :global-vars {*print-length* 100
                      *warn-on-reflection* true}

        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]

        :shorthand {. [^:lazy alembic.still/distill
                       ^:lazy alembic.still/load-project
                       ^:lazy ^:macro alembic.still/lein
                       ^:lazy cemerick.pomegranate/add-classpath
                       ^:lazy cemerick.pomegranate/get-classpath
                       ^:lazy cemerick.pomegranate/resources
                       ^:lazy clj-diff.core/diff
                       ^:lazy ^:macro clojure.core.typed/cf
                       ^:lazy clojure.core.typed/check-ns
                       ^:lazy clojure.java.javadoc/javadoc
                       ^:lazy clojure.java.shell/sh
                       ^:lazy clojure.pprint/pp
                       ^:lazy clojure.pprint/pprint
                       ^:lazy clojure.pprint/print-table
                       clojure.repl/apropos
                       clojure.repl/dir
                       clojure.repl/doc
                       clojure.repl/find-doc
                       clojure.repl/pst
                       clojure.repl/source
                       ^:lazy clojure.reflect/reflect
                       ^:lazy clojure.test/run-all-tests
                       ^:lazy clojure.test/run-tests
                       ^:lazy clojure.tools.namespace.repl/refresh
                       ^:lazy clojure.tools.namespace.repl/refresh-all
                       ^:lazy ^:macro criterium.core/bench
                       ^:lazy ^:macro criterium.core/quick-bench]}

        :aliases {"slamhound" ["run" "-m" "slam.hound"]}
        :search-page-size 50}}
