; Copyright 2021- Rahul De
;
; Use of this source code is governed by an MIT-style
; license that can be found in the LICENSE file or at
; https://opensource.org/licenses/MIT.

(ns navi.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [navi.core :as c]
   [navi.transform :as transform]))

(deftest full-test
  (testing "full route tree"
    (is (= (c/routes-from (slurp "test/api.yaml")
                          {"GetIdAndVersion" identity
                           "DeleteIdAndVersion" identity
                           "PostId" identity
                           "HealthCheck" identity
                           "GetInfoAtTime" identity
                           "GetInclusiveIntervalInteger" identity
                           "GetInclusiveIntervalNumber" identity
                           "GetMinMaxNumber" identity
                           "RunV2GraphQLQuery" identity
                           "ProvideRawData" identity})
           [["/get/{id}/and/{version}"
             {:get
              {:handler identity
               :parameters
               {:path
                [:map
                 [:id string?]
                 [:version int?]]}}
              :delete
              {:handler identity
               :parameters
               {:path
                [:map
                 [:id string?]
                 [:version int?]]}}}]
            ["/post/{id}"
             {:post
              {:handler identity
               :parameters
               {:body
                [:map
                 {:closed false}
                 [:foo uuid?]
                 [:bar inst?]
                 [:baz [:sequential number?]]]}}}]
            ["/health"
             {:get
              {:handler identity
               :parameters
               {:cookie
                [:map
                 [:last-state
                  {:optional true}
                  string?]]}}}]
            ["/info/at/{time}"
             {:get
              {:handler identity
               :parameters
               {:path [:map [:time inst?]]
                :query
                [:map
                 [:verbose {:optional true} boolean?]
                 [:foo {:optional true} [:or string? int?]]
                 [:bar {:optional true} [:and int? uuid?]]]}}}]
            ["/v1/inclusive-interval-integer"
             {:get
              {:handler identity
               :parameters
               {:query
                [:map
                 [:lower
                  {:optional true}
                  [:and int? [:>= 0M]]]
                 [:upper
                  {:optional true}
                  [:and int? [:<= 119M]]]]}}}]
            ["/v1/inclusive-interval-number"
             {:get
              {:handler identity
               :parameters
               {:query
                [:map
                 [:lower
                  {:optional true}
                  [:and number? [:>= 0M]]]
                 [:upper
                  {:optional true}
                  [:and number? [:<= 119M]]]]}}}]
            ["/v1/min-max"
             {:get
              {:handler identity
               :parameters
               {:query
                [:map
                 [:num
                  {:optional true}
                  [:and number? [:>= 0M] [:<= 100M]]]]}}}]
            ["/v2/graphql"
             {:post
              {:handler identity
               :parameters
               {:form
                [:map
                 {:closed false}
                 [:query string?]
                 [:variables
                  {:optional true}
                  string?]
                 [:operationName
                  {:optional true}
                  string?]]}}}]
            ["/raw"
             {:post
              {:handler identity
               :parameters
               {:body
                [:or nil? bytes?]}}}]]))))
