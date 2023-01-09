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
           [io.swagger.v3.oas.models.media Content StringSchema IntegerSchema ObjectSchema ArraySchema MediaType]
           [io.swagger.v3.oas.models.parameters Parameter PathParameter QueryParameter RequestBody]))

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
           (core/spec (StringSchema.)))))
  (testing "integer"
    (is (= int?
           (core/spec (IntegerSchema.)))))
  (testing "empty object"
    (is (= [:map {:closed false}]
           (core/spec (ObjectSchema.)))))
  (testing "object"
    (let [props (doto (LinkedHashMap.)
                  (.put "x" (IntegerSchema.))
                  (.put "y" (StringSchema.)))
          obj   (doto (ObjectSchema.)
                  (.setRequired ["y" "x"])
                  (.setProperties props))]
      (is (= [:map {:closed false} [:x int?] [:y string?]]
             (core/spec obj)))))
  (testing "empty array"
    (is (= [:sequential any?]
           (core/spec (ArraySchema.)))))
  (testing "array"
    (let [arr (doto (ArraySchema.)
                (.setItems (StringSchema.)))]
      (is (= [:sequential string?]
             (core/spec arr))))))

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
          operation (doto (Operation.)
                      (.setParameters [param])
                      (.setOperationId "TestOp"))
          handlers  {"TestOp" "a handler"}]
      (is (= {:handler    "a handler"
              :parameters {:path [:map [:x int?]]}}
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
