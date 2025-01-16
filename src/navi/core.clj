; Copyright 2021- Rahul De
;
; Use of this source code is governed by an MIT-style
; license that can be found in the LICENSE file or at
; https://opensource.org/licenses/MIT.

(ns navi.core
  (:import
   [io.swagger.v3.oas.models Operation PathItem PathItem$HttpMethod]
   [io.swagger.v3.oas.models.media
    ArraySchema
    BooleanSchema
    ComposedSchema
    IntegerSchema
    JsonSchema
    MediaType
    NumberSchema
    ObjectSchema
    Schema
    StringSchema
    UUIDSchema]
   [io.swagger.v3.oas.models.parameters
    HeaderParameter
    Parameter
    PathParameter
    QueryParameter
    RequestBody]
   [io.swagger.v3.oas.models.responses ApiResponse]
   [io.swagger.v3.parser OpenAPIV3Parser]
   [io.swagger.v3.parser.core.models ParseOptions]
   [java.util Map$Entry]))

(defprotocol Transformable
  (transform [_]))

;; TODO: Better
(defn wrap-map
  "Surrounds the key in a map for malli conformance"
  [k m]
  (cond-> m
    (contains? m k)
    (update-in [k] #(into [:map] %))))

(defn update-kvs
  "Update a map using `key-fn` and `val-fn`.
  Sort of like composing `update-keys` and `update-vals`.
  Unlike `update-keys` or `update-vals`, preserve `nil`s."
  [m key-fn val-fn]
  (when m
    (reduce-kv (fn kv-mapper [m k v]
                 (assoc m (key-fn k) (val-fn v)))
               {}
               m)))

;; TODO: Better
(defn ->prop-schema
  "Given a property and a required keys set, returns a malli spec.
   Intended for RequestBody"
  [required ^Map$Entry property]
  (let [k (.getKey property)
        key-schema [(keyword k)]
        key-schema (if (contains? required k)
                     key-schema
                     (conj key-schema {:optional true}))]
    (conj key-schema
          (-> property
              .getValue
              transform))))

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
              transform))))

(extend-protocol Transformable
  StringSchema
  (transform [schema]
    (let [content-fn string?
          max-length (.getMaxLength schema)
          min-length (.getMinLength schema)
          properties (cond-> nil
                       max-length (assoc :max max-length)
                       min-length (assoc :min min-length))
          pattern (some-> schema .getPattern re-pattern)
          enums (.getEnum schema)]
      (cond
        (and properties pattern)
        [:and content-fn [:string properties] pattern]

        properties
        [:and content-fn [:string properties]]

        pattern
        [:and content-fn pattern]

        (< 0 (count enums))
        (into #{} enums)

        :else
        content-fn)))

  UUIDSchema
  (transform [_] uuid?)

  IntegerSchema
  (transform [_] int?)

  NumberSchema
  (transform [_] number?)

  BooleanSchema
  (transform [_] boolean?)

  ;; TODO: Implement onrOf
  ComposedSchema
  (transform [schema]
    (let [[schemas compose-as] (cond
                                 (< 0 (count (.getAnyOf schema)))
                                 [(.getAnyOf schema) :or]

                                 (< 0 (count (.getAllOf schema)))
                                 [(.getAllOf schema) :and])]
      (->> schemas
           (map transform)
           (into [compose-as]))))

  ObjectSchema
  (transform [schema]
    (let [required (->> schema
                        .getRequired
                        (into #{}))
          schemas (->> schema
                       .getProperties
                       (map #(->prop-schema required %))
                       (into []))]
      (into [:map {:closed false}] schemas)))

  ArraySchema
  (transform [schema]
    (let [items (.getItems schema)]
      [:sequential
       (if (nil? items)
         any?
         (transform items))]))

  JsonSchema
  (transform [schema]
    (let [pred {"boolean" boolean?
                "integer" int?
                "number" number?
                "string" string?}]
      (->> schema
           .getTypes
           (map pred)
           (into [:or]))))

  Schema
  (transform [schema]
    (if-let [t (first (.getTypes schema))]
      (if (= "null" t)
        nil?
        (throw (ex-info "Unsupported schema" {:schema schema})))
      (throw (ex-info "Missing schema" {}))))

  nil
  (transform [_] any?)

  ;; TODO: Better. The extra [] is there to help with merge-with into
  PathParameter
  (transform [param]
    {:path [(->param-schema param)]})

  HeaderParameter
  (transform [param]
    {:header [(->param-schema param)]})

  QueryParameter
  (transform [param]
    {:query [(->param-schema param)]})

  ;; TODO: Handle more kinds of request-bodies
  RequestBody
  (transform [param]
    (if-let [content (.getContent param)]
      (let [^MediaType content (-> content
                                   .values
                                   .stream
                                   .findFirst
                                   .get)
            body-spec (-> content
                          .getSchema
                          transform)]
        {:body (if (.getRequired param)
                 body-spec
                 [:or nil? body-spec])})
      {})))

;;; Handle reponses

(defn handle-response-key
  "Reitit seems to want status codes of a response to be integer keys,
  rather than keyword keys or string keys (except for `:default`).
  So, convert a string to a Long if relevant.
  Else if the string is \"default\", then return `:default`, otherwise pass through.
  Arguably, all non-integer status codes should be converted to keywords."
  [s]
  (cond (re-matches #"\d{3}" s) (Long/parseLong s)
        (= "default" s) :default
        :else s))

(defn media-type->data
  "Convert a Java Schema's MediaType to a spec that Reitit will accept."
  [^MediaType mt]
  (if-let [schema (some-> mt .getSchema transform)]
    {:schema schema}
    (throw (ex-info "MediaType has no schema" {:media-type mt}))))

(defn handle-media-type-key
  "If the media type is \"default\", then return it as a keyword, otherwise pass through."
  [s]
  (if (= "default" s)
    :default
    s))

(defn response->data
  "Convert an ApiResponse to a response conforming to reitit."
  [^ApiResponse response]
  (let [orig-content (.getContent response)
        ;; If no content then use the nil? schema with a default media type.
        ;; This is a work-around for a current Reitit bug.
        ;; See https://github.com/metosin/reitit/issues/691
        content (if orig-content
                  (update-kvs orig-content handle-media-type-key media-type->data)
                  {:default {:schema nil?}})
        description (.getDescription response)]
    ;; TODO: Perhaps handle other ApiResponse fields as well?
    (cond-> {:content content}
      description (assoc :description description))))

(defn operation->data
  "Converts a Java Operation to a map of parameters, responses, schemas and handler
  that conforms to reitit."
  [^Operation op handlers]
  (try
    (let [params (into [] (.getParameters op))
          request-body (.getRequestBody op)
          params (if (nil? request-body)
                   params
                   (conj params request-body))
          schemas (->> params
                       (map transform)
                       (apply merge-with into)
                       (wrap-map :path)
                       (wrap-map :query)
                       (wrap-map :header))
          responses (-> (.getResponses op)
                        (update-kvs handle-response-key response->data))]
      (cond-> {:handler (get handlers (.getOperationId op))}
        (seq schemas) (assoc :parameters schemas)
        (seq responses) (assoc :responses responses)))
    (catch Exception e
      (throw (ex-info (str "Exception processing operation "
                           (pr-str (.getOperationId op))
                           ": " (ex-message e))
                      {:operation op}
                      e)))))

(defn path-item->data
  "Converts a path to its corresponding vector of method and the operation map"
  [^PathItem path-item handlers]
  (update-kvs (.readOperationsMap path-item)
              #(keyword (.toLowerCase (.toString ^PathItem$HttpMethod %)))
              #(operation->data % handlers)))

(defn routes-from
  "Takes in the OpenAPI JSON/YAML as string and a map of OperationId to handler fns.
   Returns the reitit route map with malli schemas"
  [^String api-spec handlers]
  (let [parse-options (doto (ParseOptions.)
                        (.setResolveFully true))
        contents (.readContents (OpenAPIV3Parser.) api-spec nil parse-options)
        paths (.getPaths (.getOpenAPI contents))]
    (mapv identity (update-kvs paths identity #(path-item->data % handlers)))))

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
