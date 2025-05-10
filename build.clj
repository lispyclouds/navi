; Copyright 2021- Rahul De
;
; Use of this source code is governed by an MIT-style
; license that can be found in the LICENSE file or at
; https://opensource.org/licenses/MIT.

(ns build
  (:require [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]))

(def lib 'org.clojars.lispyclouds/navi)

(def version "0.1.4")

(def class-dir "target/classes")

(def basis (b/create-basis {:project "deps.edn"}))

(def jar-file (format "target/%s-%s.jar" (name lib) version))

(def src-dirs ["src"])

(defn clean
  [_]
  (b/delete {:path "target"}))

(defn jar
  [_]
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis basis
                :src-dirs src-dirs
                :pom-data [[:author "Rahul De <rahul080327@gmail.com>"]
                           [:url "https://github.com/lispyclouds/navi"]
                           [:licenses
                            [:license
                             [:name "MIT"]
                             [:url "https://opensource.org/license/mit"]
                             [:distribution "repo"]]]]})
  (b/copy-dir {:src-dirs src-dirs
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))

(defn deploy
  [_]
  (dd/deploy {:installer :remote
              :sign-releases? true
              :artifact jar-file
              :pom-file (b/pom-path {:lib lib
                                     :class-dir class-dir})}))
