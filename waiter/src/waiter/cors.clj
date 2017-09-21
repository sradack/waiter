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
(ns waiter.cors
  (:require [clojure.core.async :as async]
            [waiter.utils :as utils])
  (:import java.util.regex.Pattern))

(defprotocol CorsValidator
  "A simple protocol for validating CORS requests.
   It provides two functions: one for preflight requests, and one for regular requests"
  (preflight-allowed? [this request]
    "Returns true if the preflight request is allowed.")
  (request-allowed? [this request]
    "Returns true if the CORS request is allowed."))

(defn preflight-request? [request]
  (= :options (:request-method request)))

(defn preflight-handler [cors-validator max-age request]
  (when-not (preflight-request? request)
    (throw (ex-info "Not a preflight request" {})))
  (let [{:keys [headers]} request
        {:strs [origin]} headers]
    (when-not origin
      (throw (ex-info "No origin provided" {:headers headers})))
    (when-not (preflight-allowed? cors-validator request)
      (throw (ex-info "Request not allowed" {:request request})))
    (let [{:strs [access-control-request-headers]} headers]
      {:status 200
       :headers {"access-control-allow-origin" origin
                 "access-control-allow-headers" access-control-request-headers
                 "access-control-allow-methods" "POST, GET, OPTIONS, DELETE"
                 "access-control-allow-credentials" "true"
                 "access-control-max-age" (str max-age)}})))

(defn wrap-cors [handler cors-validator]
  (fn [req]
    (let [{:keys [headers]} req
          {:strs [origin]} headers
          bless #(if (and origin (request-allowed? cors-validator req))
                   (update-in % [:headers] assoc
                              "access-control-allow-origin" origin
                              "access-control-allow-credentials" "true")
                   %)]
      (-> req
          (#(if (or (not origin) (request-allowed? cors-validator %))
              (try
                (handler %)
                (catch Throwable t
                  (throw (utils/wrap-throwable t bless))))
              (throw (ex-info (str "Cross-origin request not allowed from " origin)
                              {:status 403}))))
          (#(if (map? %)
              (bless %)
              (async/go (let [resp (async/<! %)]
                          (if (utils/throwable? resp)
                            (utils/wrap-throwable resp bless)
                            (bless resp))))))))))

(defrecord PatternBasedCorsValidator [pattern-matches?]
  CorsValidator
  (preflight-allowed? [_ request] (pattern-matches? request))
  (request-allowed? [_ request] (pattern-matches? request)))

(defn pattern-based-validator
  "Creates two validator functions, one for preflight requests, and one for regular requests.
  This validator uses the same function for both."
  [{:keys [allowed-origins]}]
  {:pre [(vector? allowed-origins)
         (every? #(instance? Pattern %) allowed-origins)]}
  (let [pattern-matches?
        (fn [{:keys [headers] :as request}]
          (let [{:strs [origin]} headers]
            (and origin
                 (or (utils/same-origin request)
                     (some #(re-matches % origin) allowed-origins)))))]
    (->PatternBasedCorsValidator pattern-matches?)))

(defrecord AllowAllCorsValidator []
  CorsValidator
  (preflight-allowed? [_ _] true)
  (request-allowed? [_ _] true))

(defn allow-all-validator
  "Creates a CORS validator that allows all cross-origin requests."
  [_]
  (->AllowAllCorsValidator))
