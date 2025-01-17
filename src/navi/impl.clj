; Copyright 2021- Rahul De
;
; Use of this source code is governed by an MIT-style
; license that can be found in the LICENSE file or at
; https://opensource.org/licenses/MIT.

(ns navi.impl
  (:require
   [navi.protocols :as p])
  (:import
   [io.swagger.v3.oas.models Operation PathItem PathItem$HttpMethod]
   [io.swagger.v3.oas.models.media MediaType]
   [io.swagger.v3.oas.models.parameters Parameter]
   [io.swagger.v3.oas.models.responses ApiResponse]
   [java.util Map$Entry]))

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
              p/transform))))

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
              p/transform))))

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
  (if-let [schema (some-> mt .getSchema p/transform)]
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
                       (map p/transform)
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
