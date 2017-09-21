
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
(ns waiter.error-handling
  (:require [clj-time.core :as t]
            [clojure.core.async :as async]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [comb.template :as template]
            [full.async :as fa]
            [waiter.async-utils :as au]
            [waiter.utils :as utils]))

(defn urls->html-links
  "Converts any URLs in a string to HTML links."
  [message]
  (when message 
    (str/replace message #"(https?://[^\s]+)" "<a href=\"$1\">$1</a>")))

(defn request->content-type
  "Determines best Content-Type for a response given a request.
  In the case of no Accept header, assume application/json if the
  request Content-Type is application/json."
  [{{:strs [accept content-type]} :headers}]
  (cond
    (and accept (str/includes? accept "application/json")) "application/json"
    (and accept (str/includes? accept "text/html")) "text/html"
    (and accept (str/includes? accept "text/plain")) "text/plain"
    (= "application/json" content-type) "application/json"
    :else "text/plain"))

(defn build-error-context
  "Creates an error context from a request and exception data."
  [^Exception e support-info {:keys [headers query-string request-method uri] :as request}]
  (let [{:strs [host x-cid]} headers
        {:keys [friendly-error-message message status] :as ex-data} (ex-data e)] 
    {:cid x-cid 
     :details ex-data
     :host host
     :message (or friendly-error-message message (.getMessage e))
     :query-string query-string
     :request-method (-> (or request-method  "") name str/upper-case)
     :status (or status 400)
     :support-info support-info
     :timestamp (utils/date-to-str (t/now))
     :uri uri}))

(defn wrap-unhandled-exception
  "Wraps any exception that doesn't already set status in a parent
  exception with a generic error message and a 500 status."
  [ex]
  (let [{:keys [status]} (ex-data ex)]
    (if status
      ex
      (ex-info "Internal error" {:status 500} ex))))

(defn exception->response
  "Converts an exception into a ring response."
  [^Exception ex support-info {:keys [] :as request}]
  (let [wrapped-ex (wrap-unhandled-exception ex)
        {:keys [headers suppress-logging]} (ex-data wrapped-ex)
        processed-headers (into {} (for [[k v] headers] [(name k) (str v)])) ]
    (when-not suppress-logging
      (log/error wrapped-ex))
    (let [content-type (request->content-type request)
          {:keys [status] :as error-context} (build-error-context wrapped-ex support-info request)]
      {:status status
       :body (case content-type
               "application/json"
               (do
                 (json/write-str {:waiter-error error-context}
                                 :value-fn utils/stringify-elements
                                 :escape-slash false))
               "text/html"
               (template/eval (slurp (io/resource "web/error.html"))
                              (-> error-context 
                                  (update :message #(urls->html-links %))
                                  (update :details #(with-out-str (pprint/pprint %)))))
               "text/plain"
               (-> (template/eval (slurp (io/resource "web/error.txt"))
                                  (-> error-context 
                                      (update :details (fn [v] 
                                                         (when v 
                                                           (-> (with-out-str (pprint/pprint v))
                                                               (str/replace #"\n" "\n  ")))))))
                   (str/replace #"\n" "\n  ")
                   (str/replace #"\n  $" "\n")))
       :headers (merge {"content-type" content-type} processed-headers)})))

(defn wrap-error-handling
  "Handles uncaught exceptions, writing an HTTP response that is content-negotiated.
  Handles both synchronous and asynchronous handlers."
  [handler support-info]
  (fn wrap-error-handling-fn [request]
    (try 
      (let [resp (handler request)]
        (if (au/chan? resp) (async/go
                              (let [ret (async/<! resp)]
                                (if (instance? Throwable ret)
                                  (exception->response ret support-info request)
                                  ret)))
          resp))
      (catch Throwable ex
        (exception->response ex support-info request)))))

