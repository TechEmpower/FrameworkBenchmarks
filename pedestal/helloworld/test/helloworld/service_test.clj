(ns helloworld.service-test
  (:require [clojure.test :refer :all]
            [io.pedestal.service.test :refer :all]
            [io.pedestal.service.http :as bootstrap]
            [helloworld.service :as service]))

(def service
  (::bootstrap/service-fn (bootstrap/create-servlet service/service)))

(deftest home-page-test
  (is (=
       (:body (response-for service :get "/"))
       "Hello World!"))
  (is (=
       (:headers (response-for service :get "/"))
       {"Content-Type" "text/html;charset=UTF-8"})))

(deftest about-page-test
  (is (.contains
       (:body (response-for service :get "/about"))
       "Clojure 1.5"))
  (is (=
       (:headers (response-for service :get "/about"))
       {"Content-Type" "text/html;charset=UTF-8"})))
