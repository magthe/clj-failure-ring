(ns simple-failure.handler
  (:require [clojure.spec.alpha :as s]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.response :as cr]
            [ring.util.response :as r]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [simple-failure.spec :as spec]
            [cheshire.core :as json]
            [failjure.core :as f]))

(s/check-asserts true)

(defrecord Response [res]
  f/HasFailed
  (failed? [self] false)
  (message [self] (str (:res self)))
  cr/Renderable
  (render [self _] self))

(defrecord Failure [mapping]
  f/HasFailed
  (failed? [_] true)
  (message [self] (str (:mapping self)))
  cr/Renderable
  (render [self _] self))

(extend-protocol cr/Renderable Exception
                 (render [self _] self))

(defn handle-div [m]
  (f/attempt-all [_ (f/try* (s/assert ::spec/div-req m))
                  val (f/try* (/ (:dividend m) (:divisor m)))]
                 (->Response {:success true
                            :result val})))

(defn handle-throw-exception [m]
  (f/attempt-all [_ (f/try* (s/assert ::spec/throw-exception-req m))]
                 (if (= "true" (:throw m))
                   (->Failure {:success false
                               :status 501
                               :case "user request"
                               :info "failure due to user's request"})
                   (->Response {:success true}))))

(defroutes app-routes
  (POST "/div" {body :body} (handle-div body))
  (POST "/throw-exception" {body :body} (handle-throw-exception body))
  (route/not-found (->Failure {:status 404 :success false :info "no such function"})))

(defn wrap-handle-failure [handler]
  (fn
    ([req]
     (f/attempt-all [result (handler req)]
                    (r/response (:res result))
                    (f/if-failed [e]
                                 (cond
                                   (instance? Failure e)
                                   (-> (r/response {:success false
                                                    :exception (:mapping e)})
                                       (r/status (get-in e [:mapping :status])))

                                   (instance? clojure.lang.ExceptionInfo e)
                                   (-> (r/response {:success false
                                                    :exception (ex-data e)})
                                       (r/status 501))

                                   :else (-> (r/response {:success false :info (str e)})
                                             (r/status 500))))))
    ([req res raise]
     ((f/attempt-all [result (handler req res raise)]
                     (r/response (:res result))
                     (f/if-failed [e]
                                  (cond
                                    (instance? Failure e)
                                    (-> (r/response {:success false
                                                     :exception (:mapping e)})
                                        (r/status (get-in e [:mapping :status])))

                                    (instance? clojure.lang.ExceptionInfo e)
                                    (-> (r/response {:success false
                                                     :exception (ex-data e)})
                                        (r/status 501))

                                    :else (-> (r/response {:success false :info (str e)})
                                              (r/status 500)))))))))

(defn wrap-handle-java-exceptions [handler]
  (fn
    ([req]
     (try (handler req)
          (catch Exception e
            (-> (r/response (json/generate-string {:success false :exception (str e)
                                                   :info "wrap-handle-exceptions-simple/1"}
                                                  {:pretty true}))
                (r/status 500)))))
    ([req res raise]
     (try (handler req res raise)
          (catch Exception e
            (-> (r/response (json/generate-string {:success false :exception (str e)
                                                   :info "wrap-handle-exceptions-simple/3"}
                                                  {:pretty true}))
                (r/status 500)))))))

(def app
  (-> app-routes
      (wrap-handle-failure)
      (wrap-defaults api-defaults)
      (wrap-json-body {:keywords? true
                       :malformed-response (-> (r/response {:success false
                                                            :info "malformed JSON in request"})
                                               (r/status 400))})
      (wrap-json-response {:pretty true})
      (wrap-handle-java-exceptions)))
