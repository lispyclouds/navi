# navi

A tiny library converting [OpenAPI](https://www.openapis.org/) route definitions to [Reitit](https://cljdoc.org/jump/release/metosin/reitit) routes.

Suitable for [spec-first](https://www.atlassian.com/blog/technology/spec-first-api-development) servers.

## Status

Experimental and work in progress. APIs subject to change.

## Features

- Read OpenAPI 3 definitions as JSON or YAML
- Remote and relative [refs](https://swagger.io/docs/specification/using-ref/)
- Request coercions powered by [Malli](https://github.com/metosin/malli)
- requestBody coercion
- The following OpenAPI types are currently supported:
  - string
  - integer
  - array
  - object

Currently unsupported:
- Response coercions
- Other coercion libs
- A lot more of the OpenAPI spec

Any contributions are much much welcome and appreciated!

## Installation
Leiningen/Boot
```clojure
[org.clojars.lispyclouds/navi "0.0.1"]
```

Clojure CLI/deps.edn
```clojure
{org.clojars.lispyclouds/navi {:mvn/version "0.0.1"}}
```

Gradle
```groovy
compile 'org.clojars.lispyclouds:navi:0.0.1'
```

Maven
```xml
<dependency>
  <groupId>org.clojars.lispyclouds</groupId>
  <artifactId>navi</artifactId>
  <version>0.0.1</version>
</dependency>
```

## Usage

Given a `api.yaml`:
```yaml
openapi: "3.0.0"

info:
  title: My calculator
  version: "0.1.0"
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
  {"AddGet"      (fn [{{{:keys [n1 n2]} :path} :parameters}]
                   {:status 200
                    :body   (+ n1 n2)})
   "AddPost"     (fn [{{{:keys [n1 n2]} :body} :parameters}]
                   {:status 200
                    :body   (+ n1 n2)})
   "HealthCheck" (fn [_]
                   {:status 200
                    :body   "Ok"})})
```

Generate the routes:
```clojure
(require '[org.clojars.lispyclouds/navi.core :as navi])

(navi/routes-from (slurp "api.yaml") handlers)
=>
[["/add/{n1}/{n2}"
  {:get
   {:handler nil,
    :parameters
    {:path
     [:map
      [:n1 #function[clojure.core/int?]]
      [:n2 #function[clojure.core/int?]]]}},
   :post
   {:handler nil,
    :parameters
    {:body
     [:map
      {:closed false}
      [:n1 #function[clojure.core/int?]]
      [:n2 #function[clojure.core/int?]]]}}}]
 ["/health" {:get {:handler nil}}]]
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

### Build Requirements
- JDK 8+
- Clojure [tools.deps](https://clojure.org/guides/getting_started)

### Running tests locally
- `clojure -M:test` to run all tests

## License

Copyright © 2020-2021 Rahul De

Distributed under the EPL License, same as Clojure. See LICENSE.
