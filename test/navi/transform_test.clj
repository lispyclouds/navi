; Copyright 2021- Rahul De
;
; Use of this source code is governed by an MIT-style
; license that can be found in the LICENSE file or at
; https://opensource.org/licenses/MIT.

(ns navi.transform-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [malli.core :as m]
   [navi.protocols :as p]
   [navi.transform])
  (:import
   [io.swagger.v3.oas.models.media
    ArraySchema
    BinarySchema
    ByteArraySchema
    ComposedSchema
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
    CookieParameter
    HeaderParameter
    PathParameter
    QueryParameter
    RequestBody]
   [java.util LinkedHashMap]))

(deftest primitives
  (testing "string"
    (is (= string? (p/transform (StringSchema.)))))
  (testing "integer"
    (is (= int? (p/transform (IntegerSchema.)))))
  (testing "number"
    (is (= number? (p/transform (NumberSchema.)))))
  (testing "null"
    (is (= nil? (p/transform (doto (Schema.) (.addType "null"))))))
  (testing "empty object"
    (is (= [:map {:closed false}]
           (p/transform (ObjectSchema.)))))
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
             (p/transform obj)))
      (is (= [:map {:closed false} [:x int?] [:y string?]]
             (p/transform obj-json)))))
  (testing "empty array"
    (is (= [:sequential any?]
           (p/transform (ArraySchema.)))))
  (testing "array"
    (let [arr (doto (ArraySchema.)
                (.setItems (StringSchema.)))
          arr-json (doto (ArraySchema.)
                     (.setItems (StringSchema.)))]
      (is (= [:sequential string?]
             (p/transform arr)))
      (is (= [:sequential string?]
             (p/transform arr-json)))))
  (testing "byte array"
    (is (= any? (p/transform (ByteArraySchema.)))))
  (testing "binary"
    (is (= any? (p/transform (BinarySchema.)))))
  (testing "nil"
    (is (= any? (p/transform nil)))))

(deftest string-formats
  (testing "uuid"
    (is (= uuid? (p/transform (UUIDSchema.)))))
  (testing "jsonschemas with multiple types"
    (let [strint (-> (JsonSchema.)
                     (.types #{"string" "integer"}))]
      (is (contains? #{[:or string? int?] [:or int? string?]} (p/transform strint)))))
  (testing "regex string"
    (let [spec (p/transform (doto (StringSchema.)
                              (.setPattern "^(\\d+)([KMGTPE]i{0,1})$")))]
      (is (m/validate spec "1024Ki"))
      (is (not (m/validate spec "1024Kib"))))
    (testing "minLength and maxLength"
      (let [spec (p/transform (doto (StringSchema.)
                                (.setMinLength (int 3))
                                (.setMaxLength (int 8))))]
        (is (not (m/validate spec "")))
        (is (not (m/validate spec "1")))
        (is (m/validate spec "123"))
        (is (m/validate spec "12345678"))
        (is (not (m/validate spec "123456789"))))))
  (testing "enums"
    (is (= [:enum "foo" "bar" "baz"]
           (p/transform (doto (StringSchema.)
                          (.setEnum ["foo" "bar" "baz"])))))))

(deftest parameters-to-malli-spec
  (testing "path"
    (let [param (doto (PathParameter.)
                  (.setName "x")
                  (.setSchema (IntegerSchema.)))]
      (is (= {:path [[:x int?]]}
             (p/transform param)))))
  (testing "query"
    (let [param (doto (QueryParameter.)
                  (.setName "x")
                  (.setRequired true)
                  (.setSchema (IntegerSchema.)))]
      (is (= {:query [[:x int?]]}
             (p/transform param)))))
  (testing "header"
    (let [param (doto (HeaderParameter.)
                  (.setName "x")
                  (.setRequired true)
                  (.setSchema (IntegerSchema.)))]
      (is (= {:header [[:x int?]]}
             (p/transform param)))))
  (testing "cookie"
    (let [param (doto (CookieParameter.)
                  (.setName "x")
                  (.setRequired true)
                  (.setSchema (IntegerSchema.)))]
      (is (= {:cookie [[:x int?]]}
             (p/transform param)))))
  (testing "required request body"
    (let [media (doto (MediaType.)
                  (.setSchema (ObjectSchema.)))
          content (doto (Content.)
                    (.put "application/json" media))
          param (doto (RequestBody.)
                  (.setRequired true)
                  (.setContent content))]
      (is (= {:body [:map {:closed false}]}
             (p/transform param)))))
  (testing "optional request body"
    (let [media (doto (MediaType.)
                  (.setSchema (ObjectSchema.)))
          content (doto (Content.)
                    (.put "application/json" media))
          param (doto (RequestBody.)
                  (.setRequired false)
                  (.setContent content))]
      (is (= {:body [:or nil? [:map {:closed false}]]}
             (p/transform param)))))
  (testing "implicitly optional request body"
    (let [media (doto (MediaType.)
                  (.setSchema (ObjectSchema.)))
          content (doto (Content.)
                    (.put "application/json" media))
          param (doto (RequestBody.)
                  (.setContent content))]
      (is (= {:body [:or nil? [:map {:closed false}]]}
             (p/transform param))))))

(deftest composed-schemas
  (testing "anyOf"
    (is (= [:or string? int?]
           (p/transform (doto (ComposedSchema.)
                          (.setAnyOf [(StringSchema.) (IntegerSchema.)]))))))
  (testing "allOF"
    (is (= [:and string? int?]
           (p/transform (doto (ComposedSchema.)
                          (.setAllOf [(StringSchema.) (IntegerSchema.)])))))))
