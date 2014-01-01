{:user {:dependencies [[slamhound "1.3.1"]
                       [org.clojure/tools.namespace "0.2.4"]
                       [clj-stacktrace "0.2.7"]]

        :plugins [[jonase/eastwood "0.0.2"]
                  [lein-cloverage "1.0.2"]
                  [lein-difftest "2.0.0"]
                  [lein-kibit "0.0.8"]
                  [lein-marginalia "0.7.1"]
                  [lein-pprint "1.1.1"]
                  [lein-swank "1.4.4"]]

        :injections [(let [orig (ns-resolve (doto 'clojure.stacktrace require)
                                            'print-cause-trace)
                           new (ns-resolve (doto 'clj-stacktrace.repl require)
                                           'pst)]
                       (alter-var-root orig (constantly (deref new))))]

        :aliases {"slamhound" ["run" "-m" "slam.hound"]}
        :source-paths ["/Users/jcf/.lein/dev"]
        :search-page-size 30}}
