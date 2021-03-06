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
(ns waiter.new-app-test
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [clojure.tools.logging :as log]
            [clojure.walk :as walk]
            [waiter.client-tools :refer :all]))

(deftest ^:parallel ^:integration-fast test-new-app
  (testing-using-waiter-url
    (let [headers {:x-kitchen-echo "true"
                   :x-waiter-name (rand-name)}
          lorem-ipsum "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
          {:keys [service-id body]}
          (make-request-with-debug-info headers #(make-kitchen-request waiter-url % :body lorem-ipsum))]
      (is (= lorem-ipsum body))
      (delete-service waiter-url service-id))))

(deftest ^:parallel ^:integration-slow test-new-app-gc
  (testing-using-waiter-url
    (let [idle-timeout-in-mins 1
          {:keys [service-id]} (make-request-with-debug-info
                                 {:x-waiter-name (rand-name)
                                  :x-waiter-idle-timeout-mins idle-timeout-in-mins}
                                 #(make-kitchen-request waiter-url %))]
      (log/debug "Waiting for" service-id "to show up...")
      (is (wait-for #(= 1 (num-instances waiter-url service-id)) :interval 1))
      (log/debug "Waiting for" service-id "to go away...")
      (is (wait-for #(= 0 (num-instances waiter-url service-id)) :interval 10))
      (delete-service waiter-url service-id))))

(deftest ^:parallel ^:integration-fast test-default-grace-period
  (testing-using-waiter-url
    (if (can-query-for-grace-period? waiter-url)
      (let [headers {:x-waiter-name (rand-name)}
            {:keys [service-id]} (make-request-with-debug-info headers #(make-kitchen-request waiter-url %))
            settings-json (waiter-settings waiter-url)
            default-grace-period (get-in settings-json [:service-description-defaults :grace-period-secs])]
        (is (= default-grace-period (service-id->grace-period waiter-url service-id)))
        (delete-service waiter-url service-id))
      (log/warn "test-default-grace-period cannot run because the target Waiter is not using Marathon"))))

(deftest ^:parallel ^:integration-fast test-custom-grace-period
  (testing-using-waiter-url
    (if (can-query-for-grace-period? waiter-url)
      (let [custom-grace-period-secs 120
            headers {:x-waiter-name (rand-name)
                     :x-waiter-grace-period-secs custom-grace-period-secs}
            {:keys [service-id]} (make-request-with-debug-info headers #(make-kitchen-request waiter-url %))]
        (is (= custom-grace-period-secs (service-id->grace-period waiter-url service-id)))
        (delete-service waiter-url service-id))
      (log/warn "test-custom-grace-period cannot run because the target Waiter is not using Marathon"))))
