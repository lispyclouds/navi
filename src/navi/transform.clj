; Copyright 2021- Rahul De
;
; Use of this source code is governed by an MIT-style
; license that can be found in the LICENSE file or at
; https://opensource.org/licenses/MIT.

(ns navi.transform
  (:require
   [navi.impl :as i]
   [navi.protocols :as p])
  (:import
   [io.swagger.v3.oas.models.media
    ArraySchema
    BinarySchema
    BooleanSchema
    ByteArraySchema
    ComposedSchema
    IntegerSchema
    JsonSchema
    MediaType
    NumberSchema
    ObjectSchema
    Schema
    StringSchema
    DateTimeSchema
    DateSchema
    UUIDSchema]
   [io.swagger.v3.oas.models.parameters
    CookieParameter
    HeaderParameter
    PathParameter
    QueryParameter
    RequestBody]))

(defn- wrap-and
  [conditions]
  (if (= 1 (count conditions))
    (first conditions)
    (into [:and] conditions)))

(defn- transform-numeric
  [main-pred schema]
  ;; The swagger-parser library does not
  ;; seem to recognize `exclusiveMinimum: true` and
  ;; `exclusiveMaximum: true`.
  (wrap-and
   (into [main-pred]
         (remove #(nil? (second %)))
         [[:>= (.getMinimum schema)]
          [:<= (.getMaximum schema)]])))

(defn- transform-object
  [^ObjectSchema schema]
  (let [required (->> schema
                      .getRequired
                      (into #{}))
        schemas (->> schema
                     .getProperties
                     (map #(i/->prop-schema required %))
                     (into []))]
    (into [:map {:closed false}] schemas)))

(defn- transform-array
  [^ArraySchema schema]
  (let [items (.getItems schema)]
    [:sequential
     (if (nil? items)
       any?
       (p/transform items))]))

(defn- transform-composed
  [^ComposedSchema schema]
  (let [[schemas compose-as] (cond
                               (< 0 (count (.getAnyOf schema)))
                               [(.getAnyOf schema) :or]

                               (< 0 (count (.getAllOf schema)))
                               [(.getAllOf schema) :and]

                               :else ;; TODO: Implement oneOf
                               (throw (IllegalArgumentException. "Unsupported composite schema. Use either anyOf, allOf")))]
    (->> schemas
         (map p/transform)
         (into [compose-as]))))

(defn- transform-string
  "Given a StringSchema or a JsonSchema that we know is string-typed,
   return a Malli schema that respects format, length constraints, pattern, and enum."
  [^Schema schema]
  (let [preds {"uuid" uuid?
               "binary" string?
               "byte" string?
               "date" inst?
               "date-time" inst?
               "password" string?
               "email" string?
               "uri" uri?
               "hostname" string?
               "ipv4" string?
               "ipv6" string?}
        content-fn (get preds (.getFormat schema) string?)
        max-length (.getMaxLength schema)
        min-length (.getMinLength schema)
        properties (cond-> nil
                     max-length (assoc :max max-length)
                     min-length (assoc :min min-length))
        pattern (some-> schema .getPattern re-pattern)
        enums (into [:enum] (.getEnum schema))]
    (cond
      (and properties pattern)
      [:and content-fn [:string properties] pattern]

      properties
      [:and content-fn [:string properties]]

      pattern
      [:and content-fn pattern]

      (< 1 (count enums))
      enums

      :else
      content-fn)))

(extend-protocol p/Transformable
  StringSchema
  (p/transform [schema]
    (transform-string schema))

  DateSchema
  (p/transform [_] inst?)

  DateTimeSchema
  (p/transform [_] inst?)

  UUIDSchema
  (p/transform [_] uuid?)

  IntegerSchema
  (p/transform [schema] (transform-numeric int? schema))

  NumberSchema
  (p/transform [schema] (transform-numeric number? schema))

  BooleanSchema
  (p/transform [_] boolean?)

  ComposedSchema
  (p/transform [schema]
    (transform-composed schema))

  ObjectSchema
  (p/transform [schema]
    (transform-object schema))

  ArraySchema
  (p/transform [schema]
    (transform-array schema))

  JsonSchema
  (p/transform [schema]
    (let [pred (fn [typ]
                 (case typ
                   "array" (transform-array schema)
                   "boolean" boolean?
                   "integer" (transform-numeric int? schema)
                   "null" nil?
                   "number" (transform-numeric number? schema)
                   "object" (transform-object schema)
                   "string" (transform-string schema)
                   (throw (IllegalArgumentException. (format "Unsupported type %s for schema %s" typ schema)))))
          types (.getTypes schema)]
      (case (count types)
        nil (throw (IllegalArgumentException. (str "Invalid schema: " schema)))
        0 (transform-composed schema)
        1 (-> types first pred)
        (into [:or] (map pred types)))))

  BinarySchema
  (p/transform [_] any?)

  ByteArraySchema
  (p/transform [_] any?)

  nil
  (p/transform [_] any?)

  Schema
  (p/transform [schema]
    (if-let [t (first (.getTypes schema))]
      (if (= "null" t)
        nil?
        (throw (Exception. (str "Unsupported schema" schema))))
      (throw (Exception. "Missing schema"))))

  ;; TODO: Better. The extra [] is there to help with merge-with into
  PathParameter
  (p/transform [param]
    {:path [(i/->param-schema param)]})

  HeaderParameter
  (p/transform [param]
    {:header [(i/->param-schema param)]})

  QueryParameter
  (p/transform [param]
    {:query [(i/->param-schema param)]})

  CookieParameter
  (p/transform [param]
    {:cookie [(i/->param-schema param)]})

  ;; TODO: Handle more kinds of request-bodies
  RequestBody
  (p/transform [param]
    (if-let [content (.getContent param)]
      (let [^MediaType content (-> content
                                   .values
                                   .stream
                                   .findFirst
                                   .get)
            body-spec (-> content
                          .getSchema
                          p/transform)]
        {:body (if (.getRequired param)
                 body-spec
                 [:or nil? body-spec])})
      {})))

(comment
  (set! *warn-on-reflection* true))
