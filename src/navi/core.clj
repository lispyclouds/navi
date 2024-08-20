; Copyright 2021- Rahul De
;
; Use of this source code is governed by an MIT-style
; license that can be found in the LICENSE file or at
; https://opensource.org/licenses/MIT.

(ns navi.core
  (:import [java.util LinkedHashMap Map$Entry]
           [io.swagger.v3.oas.models.media MediaType Schema]
           [io.swagger.v3.oas.models.parameters PathParameter
            HeaderParameter
            QueryParameter
            RequestBody
            Parameter]
           [io.swagger.v3.oas.models.responses ApiResponse]
           [io.swagger.v3.oas.models Operation
            PathItem]
           [io.swagger.v3.parser OpenAPIV3Parser]
           [io.swagger.v3.parser.core.models ParseOptions]))

(set! *warn-on-reflection* true)

(declare spec)

;; TODO: Better
(defn wrap-map
  "Surrounds the key in a map for malli conformance"
  [k m]
  (cond-> m
    (contains? m k)
    (update-in [k] #(into [:map] %))))

(defn schema->spec [^Schema schema]
  (let [types (.getTypes schema)]
    (if (= 1 (count types))
      (spec schema)
      (try
        (->> (map (fn [type]
                    (.setTypes schema #{type})
                    (spec schema))
                  types)
             (into [:or]))
        (finally
          (.setTypes schema types))))))

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
              schema->spec))))

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
              schema->spec))))

(defmulti spec
  (fn [^Schema schema]
    (if-let [x (first (.getTypes schema))]
      x
      "null")))

(defmethod spec
  "string"
  [^Schema schema]
  (if (= "uuid" (.getFormat schema))
    uuid?
    string?))

(defmethod spec
  "integer"
  [_]
  int?)

(defmethod spec
  "number"
  [_]
  number?)

(defmethod spec
  "boolean"
  [_]
  boolean?)

; Added in OpenAPI 3.1.0
(defmethod spec
  "null"
  [_]
  nil?)

(defmethod spec
  nil
  [_]
  nil?)

(defmethod spec
  "object"
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
  "array"
  [^Schema schema]
  (let [items (.getItems schema)]
    [:sequential
     (if (nil? items)
       any?
       (schema->spec items))]))

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
                               schema->spec)]
    {:body (if (.getRequired param)
             body-spec
             [:or nil? body-spec])}))

;;; Handle reponses

(defn linked-hash-map->clj-map
  "Convert a Java LinkedHashMap to a Clojure map.
  Preserve nils."
  [key-fn val-fn ^LinkedHashMap hm]
  (if (nil? hm)
    nil
    (into {}
          (map (fn lhm-mapper [map-entry]
                 (vector (key-fn (.getKey ^Map$Entry map-entry))
                         (val-fn (.getValue ^Map$Entry map-entry))))
               hm))))

(defn handle-response-key
  "Reitit seems to want status codes of a response to be integer keys,
  rather than keyword keys - except for :default.
  So, convert a string to a Long if relevant.
  If the string is \"default\" then return it as a keyword, otherwise pass through.
  Arguably, all non-integer status codes should be converted to keywords."
  [s]
  (cond (re-matches #"\d{3}" s) (Long/parseLong s)
        (= "default" s) (keyword s)
        :else s))

(defn media-type->data
  "Convert a Java Schema's MediaType to a spec that Reitit will accept."
  [^MediaType mt]
  (let [schema (.getSchema mt)]
    {:schema (spec schema)}))

(defn handle-media-type-key
  "If the media type is \"default\", then return it as a keyword, otherwise pass through."
  [s]
  (if (= "default" s)
    (keyword s)
    s))

(defn response->data
  "Convert an ApiResponse to a response conforming to reitit."
  [^ApiResponse response]
  (let [orig-content (.getContent response)
        ;; if no content then use the nil? schema with a default media type
        content (if orig-content
                  (linked-hash-map->clj-map handle-media-type-key media-type->data orig-content)
                  {:default {:schema nil?}})
        description (.getDescription response)]
    ;; TODO: Perhaps handle other ApiResponse fields as well?
    (cond-> {:content content}
      description (assoc :description description))))

(defn operation->data
  "Converts a Java Operation to a map of parameters, responses, schemas and handler
  that conforms to reitit."
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
                          (wrap-map :header))
        responses    (->> (.getResponses op)
                          (linked-hash-map->clj-map handle-response-key response->data)) ]
    (cond-> {:handler (get handlers (.getOperationId op))}
      (seq schemas) (assoc :parameters schemas)
      (seq responses) (assoc :responses responses))))

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
