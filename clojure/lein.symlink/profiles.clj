{:user {:dependencies [[clj-stacktrace "0.2.7"]
                       [im.chit/vinyasa "0.1.0"]
                       [io.aviso/pretty "0.1.8"]
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
                              'vinyasa.inject
                              'io.aviso.repl
                              'clojure.repl
                              'clojure.main
                              '[criterium.core :refer [bench quick-bench]])

                     (vinyasa.inject/inject
                       'clojure.core
                       '[[vinyasa.inject [inject inject]]
                         [vinyasa.pull [pull pull]]
                         [vinyasa.lein [lein lein]]
                         [clojure.repl apropos dir doc find-doc source
                          [root-cause cause]]
                         [clojure.tools.namespace.repl [refresh refresh]]
                         [clojure.pprint [pprint >pprint]]
                         [io.aviso.binary [write-binary >bin]]])

                     (alter-var-root #'clojure.main/repl-caught
                                     (constantly @#'io.aviso.repl/pretty-pst))
                     (alter-var-root #'clojure.repl/pst
                                     (constantly @#'io.aviso.repl/pretty-pst))]

        :aliases {"slamhound" ["run" "-m" "slam.hound"]}
        :source-paths ["/Users/jcf/.lein/dev"]
        :search-page-size 30}}
