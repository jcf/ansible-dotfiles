(ns user
  "Tools for interactive development with the REPL. This file should
  not be included in a production build of the application."
  (:require
    [clojure.main]
    [clojure.java.io :as io]
    [clojure.java.javadoc :refer (javadoc)]
    [clojure.pprint :refer (pprint)]
    [clojure.reflect :refer (reflect)]
    [clojure.repl :refer (apropos dir doc find-doc pst source)]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.test :as test]
    [clojure.tools.namespace.repl :refer (refresh refresh-all)]))

(try (require 'leiningen.hooks.difftest)
  (catch java.io.FileNotFoundException _))

(defmacro local-bindings
  "Produces a map of the names of local bindings to their values."
  []
  (let [symbols (keys &env)]
    (zipmap (map (fn [sym] `(quote ~sym)) symbols) symbols)))

(declare ^:dynamic *locals*)

(defn view-locals []
  *locals*)

(defn eval-with-locals
  "Evals a form with given locals. The locals should be a map of symbols to
  values."
  [locals form]
  (binding [*locals* locals]
    (eval
      `(let ~(vec (mapcat #(list % `(*locals* '~%)) (keys locals)))
         ~form))))

(defn dr-read
  [request-prompt request-exit]
  (let [input (clojure.main/repl-read request-prompt request-exit)]
    (if (= input '())
      request-exit
      input)))

(def ^:dynamic level 0)
(def counter (atom 1000))
(defn inc-counter []
  (swap! counter inc))

(def element (atom nil))

(def quit-dr-exception
  (proxy [Exception java.util.Enumeration] []
    (nextElement [] @element)))

(defn quit-dr [ & form]
  (reset! element (first form))
  (throw quit-dr-exception))

(def ^:dynamic  exit-dr-exception
  (Throwable. "Exiting back to main repl from debug-repl"))

(defn exit-dr []
  (throw exit-dr-exception))

(defn caught [exc]
  (cond
    (= (.getCause exc) quit-dr-exception) (throw quit-dr-exception)
    (= (.getCause exc) exit-dr-exception) (throw exit-dr-exception)
    :else (clojure.main/repl-caught exc)))

(defmacro debug-repl
  "Starts a REPL with the local bindings available."
  ([]
   `(debug-repl nil))
  ([form]
   `(let [counter# (inc-counter)
          eval-fn# (partial eval-with-locals (local-bindings))]
      (try
        (binding [level (inc level)]
          (clojure.main/repl
            :prompt #(print (str "dr-" level "-" counter# " => "))
            :eval eval-fn#
            :read dr-read
            :caught caught))
        (catch Exception e#
          (cond
            (= e# quit-dr-exception)
            (if-let [new-form# (.nextElement quit-dr-exception)]
              (eval-fn# new-form#)
              (eval-fn# ~form))
            (= e# exit-dr-exception)
            (when (> level -1)
              (throw exit-dr-exception))
            :else (throw e#)))))))
