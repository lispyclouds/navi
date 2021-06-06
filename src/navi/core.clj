; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by the terms of this license.

(ns navi.core
  (:import [java.util Map$Entry]
           [io.swagger.v3.oas.models.media Schema
                                           StringSchema
                                           IntegerSchema
                                           ObjectSchema
                                           ArraySchema
                                           MapSchema
                                           ComposedSchema
                                           NumberSchema
                                           BooleanSchema
                                           DateSchema
                                           DateTimeSchema
                                           MediaType]
           [io.swagger.v3.oas.models.parameters PathParameter
                                                QueryParameter
                                                RequestBody
                                                Parameter]
           [io.swagger.v3.oas.models Operation
                                     PathItem]
           [java.time Instant LocalDate]
           [java.time.format DateTimeParseException]
           [io.swagger.v3.oas.models.responses ApiResponses ApiResponse]
           [io.swagger.v3.oas.models Operation PathItem]
           [io.swagger.v3.parser OpenAPIV3Parser]
           [io.swagger.v3.parser.core.models ParseOptions]))

(declare spec)

;; TODO: Better
(defn wrap-map
  "Surrounds the key in a map for malli conformance"
  [k m]
  (if (contains? m k)
    (update-in m
               [k]
               #(into [:map] %))
    m))

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

(defmulti spec class)

(defmethod spec
  StringSchema
  [_]
  string?)

(defmethod spec
  IntegerSchema
  [_]
  int?)

(defmethod spec
  NumberSchema
  [_]
  number?)

(defmethod spec
  BooleanSchema
  [_]
  boolean?)

(defmethod spec
  ObjectSchema
  [^ObjectSchema schema]
  (let [required (->> schema
                      .getRequired
                      (into #{}))
        schemas  (->> schema
                      .getProperties
                      (map #(->prop-schema required %))
                      (into []))]
    (into [:map {:closed false}] schemas)))

(defmethod spec
  ArraySchema
  [^ArraySchema schema]
  (let [items (.getItems schema)]
    [:sequential
     (if (nil? items)
       any?
       (spec items))]))

(defmethod spec
  nil
  [_])

(defmethod spec
  LinkedHashMap
  [schema]
  (pp/pprint schema))

(defmethod spec
  Schema
  [schema]
  (pp/pprint (->> schema
                  .getProperties
                  spec)))

(defmethod spec
  ComposedSchema
  [^ComposedSchema schema]
  (pp/pprint schema))

(defmethod spec
  MapSchema
  [^MapSchema schema]
  (let [items (->> schema
                   ^ObjectSchema .getAdditionalProperties
                   spec)]
    items))

(defmethod spec
  DateTimeSchema
  [_]
  #(instance? Instant
              (try (Instant/parse %)
                   (catch DateTimeParseException _ false))))

(defmethod spec
  DateSchema
  [_]
  #(instance? LocalDate
              (try (LocalDate/parse %)
                   (catch DateTimeParseException _ false))))

(defmulti param->data class)

;; TODO: Better. The extra [] is there to help with merge-with into
(defmethod param->data
  PathParameter
  [param]
  {:path [(->param-schema param)]})

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
                               spec)
        maybe-body         (if (.getRequired param)
                             body-spec
                             [:or nil? body-spec])]
    {:body maybe-body}))

(defn response->data
  "Converts an ApiResponse to map."
  [^ApiResponse api-response]
  (let [^MediaType content (-> api-response
                               .getContent
                               .values
                               .stream
                               .findFirst
                               .get)
        body-spec          (-> content
                               .getSchema
                               spec)]
    {:body body-spec}))

(defn operation->data
  "Converts an Operation to map of parameters, schemas and handler conforming to reitit"
  [^Operation op handlers]
  (let [params       (into [] (.getParameters op))
        request-body (.getRequestBody op)
        params       (if (nil? request-body)
                       params
                       (conj params request-body))
        parameters   (->> params
                          (map param->data)
                          (apply merge-with into)
                          (wrap-map :path)
                          (wrap-map :query))
        responses    (->> (.getResponses op)
                          (map #(vector (.getKey ^Map$Entry %)
                                        (-> %
                                            (^ApiResponse .getValue)
                                            (response->data))))
                          (into {}))
        handler      {:handler (get handlers (.getOperationId op))}]
    (cond-> handler
      (seq parameters) (assoc :parameters parameters)
      (seq responses)  (assoc :responses responses))))

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

  (def parse-options (doto (ParseOptions.)
                        (.setResolveFully true)))

  (def api-spec (-> "compiled.json"
                    slurp))

  (->> (.readContents (OpenAPIV3Parser.) api-spec nil parse-options)
       .getOpenAPI
       .getPaths
       (mapv #(vector (.getKey ^Map$Entry %)
                      (-> ^Map$Entry %
                          .getValue
                          (path-item->data handlers))))))

