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
(ns waiter.cors-test
  (:require [clojure.core.async :as async]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [waiter.async-utils :as au]
            [waiter.cors :refer :all]
            [waiter.error-handling :as error-handling])
  (:import waiter.cors.PatternBasedCorsValidator))

(deftest pattern-validator-test
  (let [validator (pattern-based-validator {:allowed-origins [#"^http://[^\.]+\.example\.org(:80)?$"
                                                              #"^https://anotherapp.example.org:12345$"]})
        create-request-with-origin (fn [origin] {:headers {"origin" origin}})]
    (is (preflight-allowed? validator (create-request-with-origin "http://myapp.example.org")))
    (is (preflight-allowed? validator (create-request-with-origin "http://myapp.example.org:80")))
    (is (preflight-allowed? validator (create-request-with-origin "https://anotherapp.example.org:12345")))
    (is (not (preflight-allowed? validator (create-request-with-origin "http://anotherapp.example.org:12345"))))
    (is (not (preflight-allowed? validator (create-request-with-origin "http://anotherapp.example.org:12346"))))
    (is (not (preflight-allowed? validator (create-request-with-origin "http://myapp.baddomain.com"))))
    (is (not (preflight-allowed? validator (create-request-with-origin "http://myapp.baddomain.com:8080"))))
    (is (request-allowed? validator {:headers {"origin" "http://example.com"
                                               "host" "example.com"}
                                     :scheme :http}))
    (is (not (request-allowed? validator {:headers {"origin" "http://bad.example.com"
                                                    "host" "bad.example.com"}
                                          :scheme :https})))
    (is (not (request-allowed? validator {:headers {"origin" "http://bad.example.com"
                                                    "host" "good.example.com"}
                                          :scheme :http})))))

(deftest test-pattern-based-validator
  (is (thrown? Throwable (pattern-based-validator {})))
  (is (thrown? Throwable (pattern-based-validator {:allowed-origins nil})))
  (is (thrown? Throwable (pattern-based-validator {:allowed-origins ["foo"]})))
  (is (thrown? Throwable (pattern-based-validator {:allowed-origins [#"foo" "bar"]})))
  (is (thrown? Throwable (pattern-based-validator {:allowed-origins [#"foo" #"bar" "baz"]})))
  (is (instance? PatternBasedCorsValidator (pattern-based-validator {:allowed-origins [#"foo" #"bar" #"baz"]}))))

(deftest test-wrap-cors

  (testing "allow"
    (let [validator (reify CorsValidator
                      (request-allowed? [_ _] true))
          handler (-> (fn [request] {:status 200})
                      (wrap-cors validator))
          {:keys [headers]} (handler {:headers {"origin" "example.com"}})]
      (is (= "example.com" (get headers "access-control-allow-origin")))
      (is (= "true" (get headers "access-control-allow-credentials")))))

  (testing "allow async"
    (let [validator (reify CorsValidator
                      (request-allowed? [_ _] true))
          handler (-> (fn [request] (async/go {:status 200}))
                      (wrap-cors validator)
                      au/wrap-sync)
          {:keys [headers]} (handler {:headers {"origin" "example.com"}})]
      (is (= "example.com" (get headers "access-control-allow-origin")))
      (is (= "true" (get headers "access-control-allow-credentials")))))

  (testing "deny"
    (let [validator (reify CorsValidator
                      (request-allowed? [_ _] false)
                      (preflight-allowed? [_ _] false))
          handler (-> (fn [request] {:status 200})
                      (wrap-cors validator)
                      (error-handling/wrap-error-handling {}))
          {:keys [body status]} (handler {:headers {"origin" "example.com"
                                                    "accept" "application/json"}})
          json-data (try (json/read-str body)
                         (catch Exception _
                           (is false (str "not json:\n" body))))]
      (is (= status 403))
      (is (= "Cross-origin request not allowed from example.com" (get-in json-data ["waiter-error" "message"])))))

  (testing "downstream error handling sync"
    (let [validator (reify CorsValidator
                      (request-allowed? [_ _] true))
          handler (-> (fn [request] (throw (ex-info "bad" {:headers {"test" "test"}
                                                           :status 400})))
                      (wrap-cors validator)
                      (error-handling/wrap-error-handling {}))
          {:keys [body headers status]} (handler {:headers {"origin" "example.com"}})]
      (is (= "example.com" (get headers "access-control-allow-origin")))
      (is (= "true" (get headers "access-control-allow-credentials")))
      (is (= status 400))
      (is (str/includes? body "bad"))))

  (testing "downstream error handling async"
    (let [validator (reify CorsValidator
                      (request-allowed? [_ _] true))
          handler (-> (fn [request] (async/go (ex-info "bad" {:headers {"test" "test"}
                                                              :status 400})))
                      (wrap-cors validator)
                      (error-handling/wrap-error-handling {})
                      au/wrap-sync)
          {:keys [body headers status]} (handler {:headers {"origin" "example.com"}})]
      (is (= "example.com" (get headers "access-control-allow-origin")))
      (is (= "true" (get headers "access-control-allow-credentials")))
      (is (= status 400))
      (is (str/includes? body "bad")))))
