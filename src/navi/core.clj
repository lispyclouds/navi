; Copyright 2021- Rahul De
;
; Use of this source code is governed by an MIT-style
; license that can be found in the LICENSE file or at
; https://opensource.org/licenses/MIT.

(ns navi.core
  (:require
   [navi.impl :as i]
   [navi.transform]) ;; TODO: Can this be improved?
  (:import
   [io.swagger.v3.parser OpenAPIV3Parser]
   [io.swagger.v3.parser.core.models ParseOptions]))

(defn routes-from
  "Takes in the OpenAPI JSON/YAML as string and a map of OperationId to handler fns.
   Returns the reitit route map with malli schemas"
  [^String api-spec handlers]
  (let [parse-options (doto (ParseOptions.)
                        (.setResolveFully true))
        contents (.readContents (OpenAPIV3Parser.) api-spec nil parse-options)
        paths (.getPaths (.getOpenAPI contents))]
    (i/update-kvs paths identity #(i/path-item->data % handlers))))

(comment
  (require '[clojure.pprint :as pp])

  (set! *warn-on-reflection* true)

  (def handlers
    {"AddGet" (fn [{{{:keys [n1 n2]} :path} :parameters}]
                {:status 200
                 :body (+ n1 n2)})
     "AddPost" (fn [{{{:keys [n1 n2]} :body} :parameters}]
                 {:status 200
                  :body (+ n1 n2)})
     "HealthCheck" (fn [_]
                     {:status 200
                      :body "Ok"})})
  (-> "api.yaml"
      slurp
      (routes-from handlers)
      pp/pprint))
