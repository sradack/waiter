;;
;;       Copyright (c) 2017 Two Sigma Investments, LP.
;;       All Rights Reserved
;;
;;       THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF
;;       Two Sigma Investments, LP.
;;
;;       The copyright notice above does not evidence any
;;       actual or intended publication of such source code.
;;
(ns waiter.error-handling-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [waiter.error-handling :refer :all]))

(deftest test-exception->response
  (let [request {:request-method :get
                 :uri "/path"
                 :host "localhost"}]
    (testing "html response"
      (let [{:keys [body headers status]} 
            (exception->response 
              (ex-info "TestCase Exception" {:status 400})
              {}
              (assoc-in request [:headers "accept"] "text/html"))]
        (is (= 400 status))
        (is (= {"content-type" "text/html"} headers))
        (is (str/includes? body "TestCase Exception"))))
    (testing "html response with links"
      (let [{:keys [body headers status]} 
            (exception->response 
              (ex-info "TestCase Exception" {:status 400
                                             :friendly-error-message "See http://localhost/path"})
              {}
              (assoc-in request [:headers "accept"] "text/html"))]
        (is (= 400 status))
        (is (= {"content-type" "text/html"} headers))
        (is (str/includes? body "See <a href=\"http://localhost/path\">http://localhost/path</a>"))))
    (testing "plaintext response"
      (let [{:keys [body headers status]}
            (exception->response
              (ex-info "TestCase Exception" {:status 400})
              {}
              (assoc-in request [:headers "accept"] "text/plain"))]
        (is (= 400 status))
        (is (= {"content-type" "text/plain"} headers))
        (is (str/includes? body "TestCase Exception"))))
    (testing "json response"
      (let [{:keys [body headers status]}
            (exception->response 
              (ex-info "TestCase Exception" {:status 500})
              {}
              (assoc-in request [:headers "accept"] "application/json"))]
        (is (= 500 status))
        (is (= {"content-type" "application/json"} headers))
        (is (str/includes? body "TestCase Exception"))))))

(deftest test-urls->html-links
  (testing "nil"
    (is (= nil (urls->html-links nil))))
  (testing "http"
    (is (= "<a href=\"http://localhost\">http://localhost</a>"
           (urls->html-links "http://localhost"))))
  (testing "with path"
    (is (= "<a href=\"http://localhost/path\">http://localhost/path</a>"
           (urls->html-links "http://localhost/path"))))
  (testing "https"
    (is (= "<a href=\"https://localhost/path\">https://localhost/path</a>"
           (urls->html-links "https://localhost/path"))))
  (testing "mixed content"
    (is (= "hello <a href=\"https://localhost/path\">https://localhost/path</a> world"
           (urls->html-links "hello https://localhost/path world")))))

(deftest test-request->content-type
  (testing "application/json if specified"
    (is (= "application/json" (request->content-type {:headers {"accept" "application/json"}}))))
  (testing "text/html if specified"
    (is (= "text/html" (request->content-type {:headers {"accept" "text/html"}}))))
  (testing "text/plain if specified"
    (is (= "text/plain" (request->content-type {:headers {"accept" "text/plain"}}))))
  (testing "application/json if Content-Type is application/json"
    (is (= "application/json" (request->content-type {:headers {"content-type" "application/json"}}))))
  (testing "else text/plain"
    (is (= "text/plain" (request->content-type {:headers {"accept" "*/*"}})))
    (is (= "text/plain" (request->content-type {:headers {"accept" ""}})))
    (is (= "text/plain" (request->content-type {})))))
