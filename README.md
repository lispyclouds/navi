# navi

[![Tests](https://github.com/lispyclouds/navi/actions/workflows/ci.yaml/badge.svg)](https://github.com/lispyclouds/navi/actions/workflows/ci.yaml)
[![Clojars Project](https://img.shields.io/clojars/v/org.clojars.lispyclouds/navi.svg)](https://clojars.org/org.clojars.lispyclouds/navi)

A tiny library converting [OpenAPI](https://www.openapis.org/) route definitions to [Reitit](https://cljdoc.org/jump/release/metosin/reitit) routes.

Suitable for [spec-first](https://www.atlassian.com/blog/technology/spec-first-api-development) servers.

## Features

- Read OpenAPI 3 definitions as JSON or YAML
- Remote and relative [refs](https://swagger.io/docs/specification/using-ref/)
- Request and response coercions powered by [Malli](https://github.com/metosin/malli)
- requestBody coercion
- Strings with uuid and pattern types
- A large subset of OpenAPI types are currently supported, please raise an issue if something is unsupported

Currently unsupported (raise an issue if needed!):
- Other coercion libs
- `oneOf` composed schema (mostly can be handled by `anyOf`)
- Some string formats:
  - Email
  - File

Any contributions are much much welcome and appreciated!

## Installation
Leiningen/Boot
```clojure
[org.clojars.lispyclouds/navi "0.1.2"]
```

Clojure CLI/deps.edn
```clojure
{org.clojars.lispyclouds/navi {:mvn/version "0.1.2"}}
```

Gradle
```groovy
compile 'org.clojars.lispyclouds:navi:0.1.2'
```

Maven
```xml
<dependency>
  <groupId>org.clojars.lispyclouds</groupId>
  <artifactId>navi</artifactId>
  <version>0.1.2</version>
</dependency>
```

## Usage

Given a `api.yaml`:
```yaml
openapi: "3.0.0"

info:
  title: My calculator
  version: "1.0"
  description: My awesome calc!

paths:
  "/add/{n1}/{n2}":
    get:
      operationId: AddGet
      summary: Adds two numbers

      parameters:
        - name: n1
          required: true
          in: path
          description: The first number
          schema:
            type: integer
        - name: n2
          required: true
          in: path
          description: The second number
          schema:
            type: integer
    post:
      operationId: AddPost
      summary: Adds two numbers via POST

      requestBody:
        description: The numebers map
        required: true
        content:
          application/json:
            schema:
              $ref: "#/components/schemas/NumbersMap"
  "/health":
    get:
      operationId: HealthCheck
      summary: Returns Ok if all is well

components:
  schemas:
    NumbersMap:
      type: object
      required:
        - n1
        - n2
      properties:
        n1:
          type: integer
          description: The first number
        n2:
          type: integer
          description: The second number
```

A clojure map of OperationId to handler fns:
```clojure
(def handlers
  {"AddGet" (fn [{{{:keys [n1 n2]} :path} :parameters}]
              {:status 200
               :body (str (+ n1 n2))})
   "AddPost" (fn [{{{:keys [n1 n2]} :body} :parameters}]
               {:status 200
                :body (str (+ n1 n2))})
   "HealthCheck" (fn [_]
                   {:status 200
                    :body "Ok"})})
```

Generate the routes:
```clojure
(require '[navi.core :as navi])

(navi/routes-from (slurp "api.yaml") handlers)
=>
[["/add/{n1}/{n2}"
  {:get
   {:handler #function[navi.core/fn--8260],
    :parameters
    {:path
     [:map
      [:n1 #function[clojure.core/int?]]
      [:n2 #function[clojure.core/int?]]]}},
   :post
   {:handler #function[navi.core/fn--8266],
    :parameters
    {:body
     [:map
      {:closed false}
      [:n1 #function[clojure.core/int?]]
      [:n2 #function[clojure.core/int?]]]}}}]
 ["/health" {:get {:handler #function[navi.core/fn--8271]}}]]
```

Bootstrapping a Jetty server:
```clojure
(ns server.main
  (:require
   [muuntaja.core :as m]
   [navi.core :as navi]
   [reitit.coercion.malli :as malli]
   [reitit.http :as http]
   [reitit.http.coercion :as coercion]
   [reitit.http.interceptors.exception :as exception]
   [reitit.http.interceptors.muuntaja :as muuntaja]
   [reitit.http.interceptors.parameters :as parameters]
   [reitit.interceptor.sieppari :as sieppari]
   [reitit.ring :as ring]
   [ring.adapter.jetty :as jetty])
  (:gen-class))

(def server
  (http/ring-handler
   (http/router (-> "api.yaml"
                    slurp
                    (navi/routes-from handlers)) ; handlers is the map described before
                {:data {:coercion malli/coercion
                        :muuntaja m/instance
                        :interceptors [(parameters/parameters-interceptor)
                                       (muuntaja/format-negotiate-interceptor)
                                       (muuntaja/format-response-interceptor)
                                       (exception/exception-interceptor)
                                       (muuntaja/format-request-interceptor)
                                       (coercion/coerce-exceptions-interceptor)
                                       (coercion/coerce-response-interceptor)
                                       (coercion/coerce-request-interceptor)]}})
   (ring/routes
    (ring/create-default-handler
     {:not-found (constantly {:status 404
                              :headers {"Content-Type" "application/json"}
                              :body "{\"message\": \"Took a wrong turn?\"}"})}))
   {:executor sieppari/executor}))

(defn -main
  [& _]
  (jetty/run-jetty (var server)
                   {:host "0.0.0.0"
                    :port 7777
                    :join? false
                    :async? true}))
```

### Build Requirements
- JDK 8+
- Clojure [tools.deps](https://clojure.org/guides/getting_started)

### Running tests locally
- `clojure -X:test` to run all tests

## License

Copyright © 2020- Rahul De

Distributed under the MIT License. See LICENSE.
