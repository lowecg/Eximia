(ns build
  (:require
   [clojure.java.io :as io]
   [clojure.tools.build.api :as b]))

(def lib 'org.clojars.lowecg/eximia)
(def class-dir "target/classes")
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn- src-dirs []
  (->> ["src" "resources"]
       (filter #(-> % io/file .exists))
       vec))

(def pom-data
  [[:description "Fork of Eximia, a fast and small XML processor for Clojure."]
   [:url "https://github.com/lowecg/Eximia"]
   [:licenses
    [:license
     [:name "Eclipse Public License 1.0"]
     [:url "https://www.eclipse.org/legal/epl-v10.html"]]]
   [:scm
    [:url "https://github.com/lowecg/Eximia"]
    [:connection "scm:git:https://github.com/lowecg/Eximia.git"]
    [:developerConnection "scm:git:git@github.com:lowecg/Eximia.git"]
    [:tag "HEAD"]]])

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar
  [{:keys [version]
    :or   {version "0.0.0-local"}}]
  (let [srcs     (src-dirs)
        jar-file (format "target/%s-%s.jar" (name lib) version)
        pom-file (format "%s/META-INF/maven/%s/%s/pom.xml"
                         class-dir
                         (namespace lib)
                         (name lib))]
    (clean nil)
    (b/copy-dir {:src-dirs srcs
                 :target-dir class-dir})
    (b/write-pom {:class-dir class-dir
                  :lib lib
                  :version version
                  :basis @basis
                  :src-dirs srcs
                  :pom-data pom-data})
    (b/jar {:class-dir class-dir
            :jar-file jar-file})
    (println (pr-str {:jar-file jar-file
                      :pom-file pom-file
                      :version version}))
    {:jar-file jar-file
     :pom-file pom-file
     :version version}))
