; Copyright 2021- Rahul De
;
; Use of this source code is governed by an MIT-style
; license that can be found in the LICENSE file or at
; https://opensource.org/licenses/MIT.

(ns navi.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [navi.core :as core])
  (:import [java.util Map LinkedHashMap]
           [io.swagger.v3.oas.models Operation PathItem]
           [io.swagger.v3.oas.models.media Content StringSchema IntegerSchema JsonSchema
            NumberSchema ObjectSchema ArraySchema MediaType UUIDSchema Schema]
           [io.swagger.v3.oas.models.parameters Parameter PathParameter HeaderParameter QueryParameter RequestBody]))

(deftest map-to-malli-spec
  (testing "surrounding values of a clojure map to a malli map spec"
    (is (= {:path [:map [:x string?] [:y int?]]}
           (core/wrap-map :path {:path [[:x string?] [:y int?]]}))))
  (testing "surround ignores non matching key"
    (is (= {:query [:map [:x string?]]}
           (core/wrap-map :path {:query [:map [:x string?]]})))))

(deftest openapi-properties-to-malli-spec
  (testing "convert a required OpenAPI Map entry"
    (let [property (Map/entry "id" (StringSchema.))]
      (is (= [:id string?]
             (core/->prop-schema #{"id" "x"} property)))))
  (testing "convert an optional OpenAPI Map entry"
    (let [property (Map/entry "id" (StringSchema.))]
      (is (= [:id {:optional true} string?]
             (core/->prop-schema #{"x"} property))))))

(deftest openapi-parameters-to-malli-spec
  (testing "convert a required OpenAPI Parameter"
    (let [param (doto (Parameter.)
                  (.setName "x")
                  (.setRequired true)
                  (.setSchema (StringSchema.)))]
      (is (= [:x string?]
             (core/->param-schema param)))))
  (testing "convert an optional OpenAPI Map entry"
    (let [param (doto (Parameter.)
                  (.setName "x")
                  (.setSchema (StringSchema.)))]
      (is (= [:x {:optional true} string?]
             (core/->param-schema param))))))

(deftest openapi-schema-to-malli-spec
  (testing "string"
    (is (= string?
           (core/schema->spec (StringSchema.))))
    (is (= string?
           (core/schema->spec (doto (Schema.)
                        (.addType "string"))))))
  (testing "integer"
    (is (= int?
           (core/schema->spec (IntegerSchema.))))
    (is (= int?
           (core/schema->spec (doto (Schema.)
                        (.addType "integer"))))))
  (testing "number"
    (is (= number?
           (core/schema->spec (NumberSchema.))))
    (is (= number?
           (core/schema->spec (doto (Schema.)
                        (.addType "number"))))))
  (testing "null"
    (is (= nil?
           (core/schema->spec (doto (Schema.)
                        (.addType "null"))))))
  (testing "empty object"
    (is (= [:map {:closed false}]
           (core/schema->spec (ObjectSchema.))))
    (is (= [:map {:closed false}]
           (core/schema->spec (doto (Schema.)
                        (.addType "object"))))))
  (testing "object"
    (let [props (doto (LinkedHashMap.)
                  (.put "x" (IntegerSchema.))
                  (.put "y" (StringSchema.)))
          obj   (doto (ObjectSchema.)
                  (.setRequired ["y" "x"])
                  (.setProperties props))
          props-json (doto (LinkedHashMap.)
                       (.put "x" (doto (Schema.)
                                   (.addType "integer")))
                       (.put "y" (doto (Schema.)
                                   (.addType "string"))))
          obj-json   (doto (Schema.)
                       (.addType "object")
                       (.setRequired ["y" "x"])
                       (.setProperties props-json))]
      (is (= [:map {:closed false} [:x int?] [:y string?]]
             (core/schema->spec obj)))
      (is (= [:map {:closed false} [:x int?] [:y string?]]
             (core/schema->spec obj-json)))))
  (testing "empty array"
    (is (= [:sequential any?]
           (core/schema->spec (ArraySchema.))))
    (is (= [:sequential any?]
           (core/schema->spec (doto (Schema.)
                        (.addType "array"))))))
  (testing "array"
    (let [arr (doto (ArraySchema.)
                (.setItems (StringSchema.)))
          arr-json (doto (Schema.)
                     (.addType "array")
                     (.setItems (doto (Schema.)
                                  (.addType "string"))))]
      (is (= [:sequential string?]
             (core/schema->spec arr)))
      (is (= [:sequential string?]
             (core/schema->spec arr-json)))))
  (testing "uuid"
    (is (= uuid?
           (core/schema->spec (UUIDSchema.))))
    (is (= uuid?
           (core/schema->spec (doto (Schema.)
                        (.addType "string")
                        (.setFormat "uuid"))))))
  
  (testing "jsonschemas with multiple types"
    (let [strint (-> (JsonSchema.)
                   (.types #{"string" "integer"}))]
      (is (#{[:or string? int?] [:or int? string?]}
           (core/schema->spec strint))))))

(deftest parameters-to-malli-spec
  (testing "path"
    (let [param (doto (PathParameter.)
                  (.setName "x")
                  (.setSchema (IntegerSchema.)))]
      (is (= {:path [[:x int?]]}
             (core/param->data param)))))
  (testing "query"
    (let [param (doto (QueryParameter.)
                  (.setName "x")
                  (.setRequired true)
                  (.setSchema (IntegerSchema.)))]
      (is (= {:query [[:x int?]]}
             (core/param->data param)))))
  (testing "header"
    (let [param (doto (HeaderParameter.)
                  (.setName "x")
                  (.setRequired true)
                  (.setSchema (IntegerSchema.)))]
      (is (= {:header [[:x int?]]}
             (core/param->data param)))))
  (testing "required request body"
    (let [media   (doto (MediaType.)
                    (.setSchema (ObjectSchema.)))
          content (doto (Content.)
                    (.put "application/json" media))
          param   (doto (RequestBody.)
                    (.setRequired true)
                    (.setContent content))]
      (is (= {:body [:map {:closed false}]}
             (core/param->data param)))))
  (testing "optional request body"
    (let [media   (doto (MediaType.)
                    (.setSchema (ObjectSchema.)))
          content (doto (Content.)
                    (.put "application/json" media))
          param   (doto (RequestBody.)
                    (.setRequired false)
                    (.setContent content))]
      (is (= {:body [:or nil? [:map {:closed false}]]}
             (core/param->data param)))))
  (testing "implicitly optional request body"
    (let [media   (doto (MediaType.)
                    (.setSchema (ObjectSchema.)))
          content (doto (Content.)
                    (.put "application/json" media))
          param   (doto (RequestBody.)
                    (.setContent content))]
      (is (= {:body [:or nil? [:map {:closed false}]]}
             (core/param->data param))))))

(deftest openapi-operation-to-malli-spec
  (testing "OpenAPI operation to reitit ring handler"
    (let [param     (doto (PathParameter.)
                      (.setName "x")
                      (.setSchema (IntegerSchema.)))
          hparam    (doto (HeaderParameter.)
                      (.setName "y")
                      (.setSchema (StringSchema.)))
          operation (doto (Operation.)
                      (.setParameters [param hparam])
                      (.setOperationId "TestOp"))
          handlers  {"TestOp" "a handler"}]
      (is (= {:handler    "a handler"
              :parameters {:path [:map [:x int?]]
                           :header [:map [:y {:optional true} string?]]}}
             (core/operation->data operation handlers))))))

(deftest openapi-path-to-malli-spec
  (testing "OpenAPI path to reitit route"
    (let [param     (doto (PathParameter.)
                      (.setName "x")
                      (.setSchema (IntegerSchema.)))
          operation (doto (Operation.)
                      (.setParameters [param])
                      (.setOperationId "TestOp"))
          handlers  {"TestOp" "a handler"}
          path-item (doto (PathItem.)
                      (.setGet operation))]
      (is (= {:get {:handler    "a handler"
                    :parameters {:path [:map [:x int?]]}}}
             (core/path-item->data path-item handlers))))))
