; Copyright 2021- Rahul De
;
; Use of this source code is governed by an MIT-style
; license that can be found in the LICENSE file or at
; https://opensource.org/licenses/MIT.

(ns navi.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [navi.core :as c]))

(deftest full-test
  (testing "full route gen"
    (is (= (c/routes-from (slurp "test/api.yaml")
                          {"GetIdAndVersion" identity
                           "DeleteIdAndVersion" identity
                           "PostId" identity
                           "HealthCheck" identity
                           "GetInfoAtTime" identity})
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
                 [:verbose {:optional true} boolean?]]}}}]]))))
