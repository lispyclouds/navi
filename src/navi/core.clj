; Copyright 2021- Rahul De
;
; Use of this source code is governed by an MIT-style
; license that can be found in the LICENSE file or at
; https://opensource.org/licenses/MIT.

(ns navi.core
  (:import [java.util Map$Entry]
           [io.swagger.v3.oas.models.media MediaType Schema]
           [io.swagger.v3.oas.models.parameters PathParameter
            HeaderParameter
            QueryParameter
            RequestBody
            Parameter]
           [io.swagger.v3.oas.models Operation
            PathItem]
           [io.swagger.v3.parser OpenAPIV3Parser]
           [io.swagger.v3.parser.core.models ParseOptions]))

(declare spec)

;; TODO: Better
(defn wrap-map
  "Surrounds the key in a map for malli conformance"
  [k m]
  (cond-> m
    (contains? m k)
    (update-in [k] #(into [:map] %))))

;; TODO: Better
(defn ->prop-schema
  "Given a property and a required keys set, returns a malli spec.
   Intended for RequestBody"
  [required ^Map$Entry property]
  (let [k          (.getKey property)
        key-schema [(keyword k)]
        key-schema (if (contains? required k)
                     key-schema
                     (conj key-schema {:optional true}))]
    (conj key-schema
          (-> property
              .getValue
              spec))))

(defn ->param-schema
  "Given a param applies the similar logic as prop to schema
   Intended for Parameter"
  [^Parameter param]
  (let [key-spec [(-> param
                      .getName
                      keyword)]
        key-spec (if (.getRequired param)
                   key-spec
                   (conj key-spec {:optional true}))]
    (conj key-spec
          (-> param
              .getSchema
              spec))))

(defmulti spec
  (fn [^Schema schema]
    (.getTypes schema)))

(defmethod spec
  #{"string"}
  [^Schema schema]
  (if (= "uuid" (.getFormat schema))
    uuid?
    string?))

(defmethod spec
  #{"integer"}
  [_]
  int?)

(defmethod spec
  #{"number"}
  [_]
  number?)

(defmethod spec
  #{"boolean"}
  [_]
  boolean?)

(defmethod spec
  #{"object"}
  [^Schema schema]
  (let [required (->> schema
                      .getRequired
                      (into #{}))
        schemas  (->> schema
                      .getProperties
                      (map #(->prop-schema required %))
                      (into []))]
    (into [:map {:closed false}] schemas)))

(defmethod spec
  #{"array"}
  [^Schema schema]
  (let [items (.getItems schema)]
    [:sequential
     (if (nil? items)
       any?
       (spec items))]))

(defmulti param->data class)

;; TODO: Better. The extra [] is there to help with merge-with into
(defmethod param->data
  PathParameter
  [param]
  {:path [(->param-schema param)]})

(defmethod param->data
  HeaderParameter
  [param]
  {:header [(->param-schema param)]})

(defmethod param->data
  QueryParameter
  [param]
  {:query [(->param-schema param)]})

;; TODO: Handle more kinds of request-bodies
(defmethod param->data
  RequestBody
  [^RequestBody param]
  (let [^MediaType content (-> param
                               .getContent
                               .values
                               .stream
                               .findFirst
                               .get)
        body-spec          (-> content
                               .getSchema
                               spec)]
    {:body (if (.getRequired param)
             body-spec
             [:or nil? body-spec])}))

(defn operation->data
  "Converts an Operation to map of parameters, schemas and handler conforming to reitit"
  [^Operation op handlers]
  (let [params       (into [] (.getParameters op))
        request-body (.getRequestBody op)
        params       (if (nil? request-body)
                       params
                       (conj params request-body))
        schemas      (->> params
                          (map param->data)
                          (apply merge-with into)
                          (wrap-map :path)
                          (wrap-map :query)
                          (wrap-map :header))]
    (cond-> {:handler (get handlers (.getOperationId op))}
      (seq schemas)
      (assoc :parameters schemas))))

(defn path-item->data
  "Converts a path to its corresponding vector of method and the operation map"
  [^PathItem path-item handlers]
  (->> path-item
       .readOperationsMap
       (map #(vector (-> ^Map$Entry %
                         .getKey
                         .toString
                         .toLowerCase
                         keyword)
                     (-> ^Map$Entry %
                         .getValue
                         (operation->data handlers))))
       (into {})))

(defn routes-from
  "Takes in the OpenAPI JSON/YAML as string and a map of OperationId to handler fns.
   Returns the reitit route map with malli schemas"
  [^String api-spec handlers]
  (let [parse-options (doto (ParseOptions.)
                        (.setResolveFully true))]
    (->> (.readContents (OpenAPIV3Parser.) api-spec nil parse-options)
         .getOpenAPI
         .getPaths
         (mapv #(vector (.getKey ^Map$Entry %)
                        (-> ^Map$Entry %
                            .getValue
                            (path-item->data handlers)))))))

(comment
  (require '[clojure.pprint :as pp])

  (set! *warn-on-reflection* true)

  (def handlers
    {"AddGet"      (fn [{{{:keys [n1 n2]} :path} :parameters}]
                     {:status 200
                      :body   (+ n1 n2)})
     "AddPost"     (fn [{{{:keys [n1 n2]} :body} :parameters}]
                     {:status 200
                      :body   (+ n1 n2)})
     "HealthCheck" (fn [_]
                     {:status 200
                      :body   "Ok"})})
  (-> "api.yaml"
      slurp
      (routes-from handlers)
      pp/pprint))
