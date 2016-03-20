(defproject org-struct "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clocop "0.2.0"]
                 [org.gnu.glpk/glpk-java "1.4.0"]
                 ;[expresso "0.2.2-SNAPSHOT"]
                 [prismatic/schema "1.0.5"]
                 [org.clojure/core.match "0.3.0-alpha4"]]
  ;; for glpk-java
  :repositories {"XypronRelease" "http://rsync.xypron.de/repository"})
