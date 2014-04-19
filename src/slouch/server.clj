(ns slouch.server
  (:require [slouch.common :as common]
            [slouch.serialization :as serial]))

(defn- ns-funcs [n]
  (let [nsname (ns-name n)]
    (into {}
          (for [[k v] (ns-publics n)
                :when (and (fn? @v)
                           (:export (meta v)))]
            [(str nsname "/" (name k)) v]))))

(defn wrap-slouch-fn
  [handler funcs]
  (fn [{uri :uri :as request}]
    (let [fn-name (subs uri 1)]
      (if-let [f (get funcs fn-name)]
        (handler (assoc-in request [:slouch :fn] f))
        {:status (:not-found common/response-codes)
         :slouch {:result uri}}))))

;; TODO: Handle serialization exceptions
(defn wrap-slouch-serial
  [handler]
  (fn [{body :body :as request}]
      (let [args (serial/deserialize body)
            response (handler (assoc-in request [:slouch :args] args))
            result (get-in response [:slouch :result])]
        (assoc response :body (serial/serialize result)))))

(defn handle-request
  [{{f :fn args :args} :slouch :as request}]
  (try
    (let [result (let [r (apply f args)]
                   (if (seq? r) (doall r) r))]
      {:status (:success common/response-codes)
       :slouch {:result result}})
    (catch Throwable t
      {:status (:exception common/response-codes)
       :slouch {:result t}})))

(defn build-remotes
  [service-discovery http-post funcs]
  (->> 
    (for [[path var] funcs
          :let [{:keys [ns name] :as metadata} (meta var)
                name-sym (symbol (str name "-remote"))]]
      `((ns ~(ns-name ns))
        (defn ~name-sym
          [& args#]
          (let [base# (~service-discovery)
                resp# (~http-post base# (pr-str args#))
                ]
            (if (<= 200 (:status resp#) 299)
              (:body resp#)
              (throw (ex-info (str ~(str "Error when calling " name-sym " with args ") args#) resp#)))))
        (alter-meta! (var ~name-sym) merge '~(select-keys metadata [:arglists :column :line :file]))))
    (apply concat)
    (cons `do)))

;(meta #'build-remotes)
;(clojure.pprint/pprint(build-remotes '(constantly "http://localhost:8080") '(fn [uri body] (clj-http.client/post uri {:body body})) (ns-funcs 'slouch.server)))
;(meta #'foo-remote)

(defn handler [exposed-ns & {:keys [interceptors]}]
  ;; TODO: use core.cache for ns-funcs, to get automatic reloading
  (let [funcs (ns-funcs exposed-ns)]
    (-> handle-request
        (wrap-slouch-fn funcs)
        wrap-slouch-serial)))

(defn ^:export foo
  [a b]
  (+ a b))
