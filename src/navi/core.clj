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
    (->> #(i/path-item->data % handlers)
         (i/update-kvs paths identity)
         (mapv identity))))

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
  (def routes (-> "newopenapispec.yaml"
                  slurp
                  (routes-from handlers)
                  )))

   

(defn function->symbol [x]
  (if (fn? x)
    (case x
      clojure.core/string? 'string?
      clojure.core/int? 'int?
      clojure.core/boolean? 'boolean?
      clojure.core/inst? 'inst?
      (-> x
          .getClass
          .getName
          clojure.lang.Compiler/demunge
          symbol))
    x))

(defn routes->serializable 
  "Convert routes data structure to serializable form"
  [routes]
  (clojure.walk/postwalk function->symbol routes))

(defn save-routes! [routes filename]
  (spit filename 
        (with-out-str 
          (clojure.pprint/pprint 
            (routes->serializable routes)))))

;; Usage in REPL:
(save-routes! routes "routes2.edn")

(defn read-routes [filename]
  (clojure.edn/read-string 
    (slurp filename)))

(def ole (read-routes "routes.edn"))