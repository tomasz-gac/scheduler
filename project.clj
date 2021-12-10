(defproject scheduler "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/core.logic "1.0.0"]
                 [org.clojure/core.typed.rt "0.6.0"]
                 [org.typedclojure/typed.clj.checker "1.0.19"]]
  :plugins [[lein-typed "0.4.6"]]
  :repl-options {:init-ns scheduler.core})
