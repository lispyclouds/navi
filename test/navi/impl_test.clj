; Copyright 2021- Rahul De
;
; Use of this source code is governed by an MIT-style
; license that can be found in the LICENSE file or at
; https://opensource.org/licenses/MIT.

(ns navi.impl-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [navi.core :as navi]
   [navi.impl :as i]
   [clojure.java.io :as io])
  (:import
   [clojure.lang ExceptionInfo]
   [io.swagger.v3.oas.models Operation PathItem]
   [io.swagger.v3.oas.models.media
    Content
    IntegerSchema
    MediaType
    ObjectSchema
    StringSchema]
   [io.swagger.v3.oas.models.parameters HeaderParameter Parameter PathParameter]
   [io.swagger.v3.oas.models.responses ApiResponse ApiResponses]
   [java.util Map]))

(deftest map-to-malli-spec
  (testing "surrounding values of a clojure map to a malli map spec"
    (is (= {:path [:map [:x string?] [:y int?]]}
           (i/wrap-map :path {:path [[:x string?] [:y int?]]}))))
  (testing "surround ignores non matching key"
    (is (= {:query [:map [:x string?]]}
           (i/wrap-map :path {:query [:map [:x string?]]})))))

(deftest openapi-properties-to-malli-spec
  (testing "convert a required OpenAPI Map entry"
    (let [property (Map/entry "id" (StringSchema.))]
      (is (= [:id string?]
             (i/->prop-schema #{"id" "x"} property)))))
  (testing "convert an optional OpenAPI Map entry"
    (let [property (Map/entry "id" (StringSchema.))]
      (is (= [:id {:optional true} string?]
             (i/->prop-schema #{"x"} property))))))

(deftest openapi-parameters-to-malli-spec
  (testing "convert a required OpenAPI Parameter"
    (let [param (doto (Parameter.)
                  (.setName "x")
                  (.setRequired true)
                  (.setSchema (StringSchema.)))]
      (is (= [:x string?]
             (i/->param-schema param)))))
  (testing "convert an optional OpenAPI Map entry"
    (let [param (doto (Parameter.)
                  (.setName "x")
                  (.setSchema (StringSchema.)))]
      (is (= [:x {:optional true} string?]
             (i/->param-schema param))))))

(deftest responses-to-malli-spec
  (testing "empty response"
    (let [response (ApiResponse.)]
      (is (= {:content {:default {:schema nil?}}}
             (i/response->data response)))))
  (testing "default media type"
    (let [media (doto (MediaType.)
                  (.setSchema (StringSchema.)))
          content (doto (Content.)
                    (.put "default" media))
          response (doto (ApiResponse.)
                     (.setContent content))]
      (is (= {:content {:default {:schema string?}}}
             (i/response->data response)))))
  (testing "json object response"
    (let [media (doto (MediaType.)
                  (.setSchema (ObjectSchema.)))
          content (doto (Content.)
                    (.put "application/json" media))
          response (doto (ApiResponse.)
                     (.setContent content))]
      (is (= {:content {"application/json" {:schema [:map {:closed false}]}}}
             (i/response->data response))))))

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
             (i/operation->data operation handlers))))))

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
           (i/operation->data operation handlers))
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
             (i/path-item->data path-item handlers))))))
(defn find-route [rts path method]
        (some (fn [[p r]]
                (when (= p path)
                  (get r method)))
              rts))

(deftest security-requirements-test
  (testing "Verifying security requirements from security-users.yml"
    ;; A dummy map of operationId to handler (the actual function doesn't matter for this test).
    (let [handlers {"listUsers"          (constantly :ok)
                    "listUsersSingle"    (constantly :ok)
                    "listUsersNoScope"   (constantly :ok)
                    "listUsersNoSecurity" (constantly :ok)}
          api-spec (slurp (io/resource "security-users.yml"))
          routes   (navi/routes-from api-spec handlers)]
      
      (testing "multiple security schemes"
        (let [route (find-route routes "/users" :get)]
          (is (some? route) "Should have found /users GET route")
          (is (= [["sessionCookieAuth" ["read:user"]]
                  ["test" ["one:two"]]]
                 (:security route)))))

      (testing "single security scheme with scopes"
        (let [route (find-route routes "/users-single-scheme" :get)]
          (is (some? route) "Should have found /users-single-scheme GET route")
          (is (= [["sessionCookieAuth" ["read:user"]]]
                 (:security route)))))

      (testing "single security scheme without scopes"
        (let [route (find-route routes "/users-no-scope" :get)]
          (is (some? route) "Should have found /users-no-scope GET route")
          (is (= [["sessionCookieAuth" []]]
                 (:security route))
              "No scopes should yield an empty vector for that scheme")))

      (testing "no security block"
        (let [route (find-route routes "/users-no-security" :get)]
          (is (some? route) "Should have found /users-no-security GET route")
          (is (nil? (:security route))
              "Route with no security block should not have :security key"))))))
