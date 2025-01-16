; Copyright 2021- Rahul De
;
; Use of this source code is governed by an MIT-style
; license that can be found in the LICENSE file or at
; https://opensource.org/licenses/MIT.

(ns navi.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [malli.core :as m]
   [navi.core :as core])
  (:import
   [clojure.lang ExceptionInfo]
   [io.swagger.v3.oas.models Operation PathItem]
   [io.swagger.v3.oas.models.media
    ArraySchema
    Content
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
   [io.swagger.v3.oas.models.responses ApiResponse ApiResponses]
   [java.util LinkedHashMap Map]))

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
    (is (= string? (core/transform (StringSchema.)))))
  (testing "integer"
    (is (= int? (core/transform (IntegerSchema.)))))
  (testing "number"
    (is (= number? (core/transform (NumberSchema.)))))
  (testing "null"
    (is (= nil? (core/transform (doto (Schema.) (.addType "null"))))))
  (testing "empty object"
    (is (= [:map {:closed false}]
           (core/transform (ObjectSchema.)))))
  (testing "object"
    (let [props (doto (LinkedHashMap.)
                  (.put "x" (IntegerSchema.))
                  (.put "y" (StringSchema.)))
          obj (doto (ObjectSchema.)
                (.setRequired ["y" "x"])
                (.setProperties props))
          props-json (doto (LinkedHashMap.)
                       (.put "x" (IntegerSchema.))
                       (.put "y" (StringSchema.)))
          obj-json (doto (ObjectSchema.)
                     (.setRequired ["y" "x"])
                     (.setProperties props-json))]
      (is (= [:map {:closed false} [:x int?] [:y string?]]
             (core/transform obj)))
      (is (= [:map {:closed false} [:x int?] [:y string?]]
             (core/transform obj-json)))))
  (testing "empty array"
    (is (= [:sequential any?]
           (core/transform (ArraySchema.)))))
  (testing "array"
    (let [arr (doto (ArraySchema.)
                (.setItems (StringSchema.)))
          arr-json (doto (ArraySchema.)
                     (.setItems (StringSchema.)))]
      (is (= [:sequential string?]
             (core/transform arr)))
      (is (= [:sequential string?]
             (core/transform arr-json)))))
  (testing "uuid"
    (is (= uuid? (core/transform (UUIDSchema.)))))
  (testing "jsonschemas with multiple types"
    (let [strint (-> (JsonSchema.)
                     (.types #{"string" "integer"}))]
      (is (contains? #{[:or string? int?] [:or int? string?]} (core/transform strint)))))
  (testing "regex string"
    (let [spec (core/transform (doto (StringSchema.)
                                 (.setPattern "^(\\d+)([KMGTPE]i{0,1})$")))]
      (is (m/validate spec "1024Ki"))
      (is (not (m/validate spec "1024Kib"))))
    (testing "minLength and maxLength"
      (let [spec (core/transform (doto (StringSchema.)
                                   (.setMinLength (int 3))
                                   (.setMaxLength (int 8))))]
        (is (not (m/validate spec "")))
        (is (not (m/validate spec "1")))
        (is (m/validate spec "123"))
        (is (m/validate spec "12345678"))
        (is (not (m/validate spec "123456789")))))))

(deftest responses-to-malli-spec
  (testing "empty response"
    (let [response (ApiResponse.)]
      (is (= {:content {:default {:schema nil?}}}
             (core/response->data response)))))
  (testing "default media type"
    (let [media (doto (MediaType.)
                  (.setSchema (StringSchema.)))
          content (doto (Content.)
                    (.put "default" media))
          response (doto (ApiResponse.)
                     (.setContent content))]
      (is (= {:content {:default {:schema string?}}}
             (core/response->data response)))))
  (testing "json object response"
    (let [media (doto (MediaType.)
                  (.setSchema (ObjectSchema.)))
          content (doto (Content.)
                    (.put "application/json" media))
          response (doto (ApiResponse.)
                     (.setContent content))]
      (is (= {:content {"application/json" {:schema [:map {:closed false}]}}}
             (core/response->data response))))))

(deftest parameters-to-malli-spec
  (testing "path"
    (let [param (doto (PathParameter.)
                  (.setName "x")
                  (.setSchema (IntegerSchema.)))]
      (is (= {:path [[:x int?]]}
             (core/transform param)))))
  (testing "query"
    (let [param (doto (QueryParameter.)
                  (.setName "x")
                  (.setRequired true)
                  (.setSchema (IntegerSchema.)))]
      (is (= {:query [[:x int?]]}
             (core/transform param)))))
  (testing "header"
    (let [param (doto (HeaderParameter.)
                  (.setName "x")
                  (.setRequired true)
                  (.setSchema (IntegerSchema.)))]
      (is (= {:header [[:x int?]]}
             (core/transform param)))))
  (testing "required request body"
    (let [media (doto (MediaType.)
                  (.setSchema (ObjectSchema.)))
          content (doto (Content.)
                    (.put "application/json" media))
          param (doto (RequestBody.)
                  (.setRequired true)
                  (.setContent content))]
      (is (= {:body [:map {:closed false}]}
             (core/transform param)))))
  (testing "optional request body"
    (let [media (doto (MediaType.)
                  (.setSchema (ObjectSchema.)))
          content (doto (Content.)
                    (.put "application/json" media))
          param (doto (RequestBody.)
                  (.setRequired false)
                  (.setContent content))]
      (is (= {:body [:or nil? [:map {:closed false}]]}
             (core/transform param)))))
  (testing "implicitly optional request body"
    (let [media (doto (MediaType.)
                  (.setSchema (ObjectSchema.)))
          content (doto (Content.)
                    (.put "application/json" media))
          param (doto (RequestBody.)
                  (.setContent content))]
      (is (= {:body [:or nil? [:map {:closed false}]]}
             (core/transform param))))))

(deftest openapi-operation-to-malli-spec
  (testing "OpenAPI operation to reitit ring handler"
    (let [param (doto (PathParameter.)
                  (.setName "x")
                  (.setSchema (IntegerSchema.)))
          hparam (doto (HeaderParameter.)
                   (.setName "y")
                   (.setSchema (StringSchema.)))
          response (doto (ApiResponse.)
                     (.setContent (doto (Content.)
                                    (.put "application/json"
                                          (doto (MediaType.)
                                            (.setSchema (ObjectSchema.)))))))
          responses (doto (ApiResponses.)
                      (.put "200" response))
          operation (doto (Operation.)
                      (.setParameters [param hparam])
                      (.setResponses responses)
                      (.setOperationId "TestOp"))
          handlers {"TestOp" "a handler"}]
      (is (= {:handler "a handler"
              :parameters {:path [:map [:x int?]]
                           :header [:map [:y {:optional true} string?]]}
              :responses {200 {:content {"application/json" {:schema [:map {:closed false}]}}}}}
             (core/operation->data operation handlers))))))

(deftest openapi-operation-to-malli-spec-missing-schema
  (testing "Missing response schema results in an informative error"
    (let [param (doto (PathParameter.)
                  (.setName "x")
                  (.setSchema (IntegerSchema.)))
          hparam (doto (HeaderParameter.)
                   (.setName "y")
                   (.setSchema (StringSchema.)))
          response (doto (ApiResponse.)
                     (.setContent (doto (Content.)
                                    (.put "application/json" (MediaType.)))))
          responses (doto (ApiResponses.)
                      (.put "200" response))
          operation (doto (Operation.)
                      (.setParameters [param hparam])
                      (.setResponses responses)
                      (.setOperationId "TestOp"))
          handlers {"TestOp" "a handler"}]
      (is (thrown-with-msg?
           ExceptionInfo
           #".*TestOp.*schema"
           (core/operation->data operation handlers))
          "Error message contains operation name and mentions the missing schema"))))

(deftest openapi-path-to-malli-spec
  (testing "OpenAPI path to reitit route"
    (let [param (doto (PathParameter.)
                  (.setName "x")
                  (.setSchema (IntegerSchema.)))
          operation (doto (Operation.)
                      (.setParameters [param])
                      (.setOperationId "TestOp"))
          handlers {"TestOp" "a handler"}
          path-item (doto (PathItem.)
                      (.setGet operation))]
      (is (= {:get {:handler "a handler"
                    :parameters {:path [:map [:x int?]]}}}
             (core/path-item->data path-item handlers))))))
