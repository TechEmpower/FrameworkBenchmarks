(ns pedestal.pdg
  (:gen-class)
  (:require [io.pedestal.http :as http]
            [io.pedestal.http.route :as route]
            [io.pedestal.interceptor.chain :as chain]
            ;[io.pedestal.http.request :as request]
            ;[io.pedestal.http.body-params :as body-params]
            [clojure.java.jdbc :as jdbc]
            [cheshire.core :as json]
            [hiccup.core :as hiccup]
            [hiccup.util]
            [hiccup.page])
  (:import (java.nio ByteBuffer)
           (java.nio.charset StandardCharsets)
           (java.net InetSocketAddress)
           (org.eclipse.jetty.server Request
                                     Server)
           (org.eclipse.jetty.server.handler AbstractHandler)
           (org.eclipse.jetty.util Callback
                                   BufferUtil)
           (javax.servlet.http HttpServletRequest
                               HttpServletResponse)
           (com.zaxxer.hikari HikariConfig
                              HikariDataSource)))

;; This is based on the "Fast Pedestal" sample found in Pedestal's repo
;;  You can see the details here: https://github.com/pedestal/pedestal/tree/master/samples/fast-pedestal

;; THIS IS A BENCHMARK !!!
;; -----------------------
;; The global var settings here are strongly discouraged for production apps

;; Auxiliary functions
;; ---------------------------
(defn sanitize-queries-param
  "Sanitizes the `queries` parameter. Clamps the value between 1 and 500.
  Invalid (string) values become 1."
  [request]
  (let [n ^long (try (Integer/parseInt (get-in request [:params :queries] "1"))
                     (catch Exception e 1))] ; default to 1 on parse failure
    (cond
      (< ^long n 1) 1
      (> ^long n 500) 500
      :else n)))

(defn json-buffer
  [x]
  (BufferUtil/toBuffer (json/generate-string x) StandardCharsets/UTF_8))

(defn hikari-data-source []
  (let [config (doto (HikariConfig.)
                 (.setAutoCommit true)
                 (.setReadOnly false)
                 (.setConnectionTimeout 30000)
                 (.setValidationTimeout 5000)
                 (.setIdleTimeout 600000)
                 (.setMaxLifetime 1800000)
                 (.setMinimumIdle 10)
                 (.setMaximumPoolSize 256)
                 (.setPoolName "db-pool")
                 (.setAllowPoolSuspension false)
                 (.setUsername "benchmarkdbuser")
                 (.setPassword "benchmarkdbpass")
                 (.setJdbcUrl "jdbc:mysql://tfb-database:3306/hello_world?useSSL=false&useServerPrepStmts=true&cachePrepStmts=true")
                 (.setRegisterMbeans false))]
    (HikariDataSource. config)))

(def db (delay {:datasource (hikari-data-source)}))

;; Handlers
;; ---------------------------
(def hello-buffer (BufferUtil/toBuffer "Hello, World!" StandardCharsets/UTF_8))
(defn home-page
  [request]
  {:body (.slice ^ByteBuffer hello-buffer)
   :headers {"Content-Type" "text/plain"}
   :status 200})

(def json-page-buffer (json-buffer {:message "Hello, World!"}))
(def json-headers {"Content-Type" "application/json"})
(defn json-page
  [request]
  {:body (.slice ^ByteBuffer json-page-buffer)
   :headers json-headers
   :status 200})

(defn get-world-raw
  [^long world-id]
  (first (jdbc/query @db ["select * from world where id = ?" world-id])))

(defn get-worlds-raw
  "Run query repeatedly, return an array"
  [n-queries]
  (mapv get-world-raw (repeatedly n-queries #(unchecked-inc ^long (rand-int 9999)))))

(defn get-all-fortunes-raw
  "Query all Fortune records from the database using JDBC."
  []
  (jdbc/query @db ["select * from fortune"]))

(defn get-fortunes
  "Fetch the full list of Fortunes from the database, sort them by the fortune
  message text, and then return the results."
  []
  (sort-by :message
    (conj
      (get-all-fortunes-raw)
      {:id 0
       :message "Additional fortune added at request time."})))

(def fortunes-xf (map (fn [x]
                        [:tr
                         [:td (:fortune/id x)]
                         [:td (hiccup.util/escape-html (:fortune/message x))]])))
(defn fortunes-hiccup
  "Render the given fortunes to simple HTML using Hiccup."
  [fortunes]
  (hiccup.page/html5
    [:head
     [:title "Fortunes"]]
    [:body
     (into [:table
            [:tr
             [:th "id"]
             [:th "message"]]]
           fortunes-xf
           fortunes)]))

(def base-fortune-pre "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>")
(def base-fortune-post "</table></body></html>")
(defn fortunes-str
  "The HTML bit for this is very very small;
  Opt to create the HTML string by hand in a tight loop rather than using Hiccup"
  [fortunes]
  (let [sb (StringBuilder. ^String base-fortune-pre)]
    (doseq [{:keys [id message]} fortunes]
      (.append sb "<tr><td>")
      (.append sb (str id))
      (.append sb "</td><td>")
      (dotimes [c-idx (count message)]
        (let [c (.charAt ^String message c-idx)]
          (case c
            \& (.append sb "&amp;")
            \" (.append sb "&quot;")
            \' (.append sb "&apos;")
            \< (.append sb "&lt;")
            \> (.append sb "&gt;")
            (.append sb c))))
      (.append sb "</td></tr>"))
    (.append sb base-fortune-post)
    (.toString sb)))

(defn update-and-persist-raw
  "Using JDBC: Changes the :randomNumber of a number of world entities.
  Persists the changes to sql then returns the updated entities"
  [queries]
  ;; TODO: This should be refactored to be smarter about transducers
  (let [world (mapv #(assoc % :randomnumber (unchecked-inc ^long (rand-int 9999))) (get-worlds-raw queries))]
    (doseq [{:keys [id randomnumber]} world]
      (jdbc/update!
       @db
       :world {:randomnumber randomnumber}
       ["id = ?" id]))
    world))

(defn single-query
  "Test 2: Single database query"
  [request]
  {:body (json-buffer (get-world-raw (unchecked-inc ^long (rand-int 10000))))
   :headers json-headers
   :status 200})

(defn multiple-queries
  "Test 3: Multiple database queries"
  [request]
  (let [n (sanitize-queries-param request)
        b (json-buffer (get-worlds-raw n))]
    {:body b
     ;:headers (assoc json-headers "Content-Length" (.position ^ByteBuffer b))
     :headers json-headers
     :status 200}))

(defn fortune
  "Test 4: Fortunes"
  [request]
  (let [^String s (fortunes-str (get-fortunes))]
    {:body (BufferUtil/toBuffer s StandardCharsets/UTF_8)
     :headers {"Content-Type" "text/html;charset=UTF-8"}
     :status 200}))

(defn db-updates
  "Test 5: Database updates"
  [request]
  (let [n (sanitize-queries-param request)
        b (json-buffer (update-and-persist-raw n))]
    {:body b
     ;:headers (assoc json-headers "Content-Length" (.position ^ByteBuffer b))
     :headers json-headers
     :status 200}))

(def routes #{["/plaintext" :get `home-page]
              ["/json"      :get `json-page]
              ["/db"        :get `single-query]
              ["/queries"   :get [route/query-params `multiple-queries]]
              ["/fortunes"  :get `fortune]
              ["/updates"   :get [route/query-params `db-updates]]})


;; Usually we'd use the request/ContainerRequest protocol,
;; but we're instead going to use direct interop to Jetty.
;; We can just create something that is "request like" with only the required keys.
;;
;; We'll need a function to connect the Interceptor Chain to Jetty.
;; In Pedestal, this piece is called a "Chain Provider"
;; ----------------------------------------------------

(defn direct-jetty-provider
  "Given a service-map,
  provide all necessary functionality to execute the interceptor chain,
  including extracting and packaging the base :request into context.
  These functions/objects are added back into the service map for use within
  the server-fn.
  See io.pedestal.http.impl.servlet-interceptor.clj as an example.

  Interceptor chains:
   * Terminate based on the list of :terminators in the context.
   * Call the list of functions in :enter-async when going async.  Use these to toggle async mode on the container
   * Will use the fn at :async? to determine if the chain has been operating in async mode (so the container can handle on the outbound)"
  [service-map]
  (let [interceptors (::http/interceptors service-map)]
    ;; In this case, `proxy` will provide nearly the same class as `gen-class`
    (assoc service-map ::handler (proxy [AbstractHandler] []
                                   (handle [target ^Request base-request servlet-req servlet-resp]
                                     ;; This is where you'd build a context and execute interceptors
                                     (let [resp (.getResponse base-request)
                                           initial-context {:request {:query-string     (.getQueryString base-request)
                                                                      :request-method   (keyword (.toLowerCase ^String (.getMethod base-request)))
                                                                      :body             (.getInputStream base-request)
                                                                      :path-info        target ; Could also use (.getRequestURI base-request)
                                                                      :async-supported? (.isAsyncSupported base-request)}}
                                           resp-ctx (chain/execute initial-context
                                                                       interceptors)]
                                       (.setContentType resp (get-in resp-ctx [:response :headers "Content-Type"]))
                                       ;(.setStatus resp (get-in resp-ctx [:response :status]))
                                       (.setStatus resp 200)
                                       ;; All of our bodies are unique ByteBuffers...
                                       (.sendContent (.getHttpOutput resp)
                                                     ^ByteBuffer (get-in resp-ctx [:response :body])
                                                     ;Callback/NOOP ;; The payloads are so small, going non-blocking may cause additional overhead
                                                     )
                                       ;; If your bodies are static ByteBuffers, you'll need to slice them
                                       ;(.sendContent (.getHttpOutput resp)
                                       ;              (.slice ^ByteBuffer (get-in resp-ctx [:response :body])))
                                       ;; If your bodies are strings, you can create ByteBuffers from them
                                       ;; (.sendContent (.getHttpOutput resp)
                                       ;;               (BufferUtil/toBuffer (get-in resp-ctx [:response :body] StandardCharsets/UTF_8))
                                       ;;               ;Callback/NOOP ;; The payloads are so small, going non-blocking may cause additional overhead
                                       (.setHandled base-request true)))))))

(defn jetty-server-fn
  "Given a service map (with interceptor provider established) and a server-opts map,
  Return a map of :server, :start-fn, and :stop-fn.
  Both functions are 0-arity"
  [service-map server-opts]
  (let [handler (::handler service-map)
        {:keys [host port join?]
         :or {host "0.0.0.0"
              port 8080
              join? false}} server-opts
        addr (InetSocketAddress. ^String host ^int port)
        server (doto (Server. addr)
                 (.setHandler handler))]
    {:server server
     :start-fn (fn []
                 (.start server)
                 (when join? (.join server))
                 server)
     :stop-fn (fn []
                (.stop server)
                server)}))

;; Consumed by peddemo.server/create-server
;; See http/default-interceptors for additional options you can configure
(def service
  {:env :prod
   ::http/interceptors [;http/log-request
                        ;http/not-found
                        ;route/query-params ;; We'll do this per-route
                        ;(middleware/file-info)
                        ;; We don't have any resources
                        ;;  -- we could also use a ServletFilter to avoid calling
                        ;;     into the application/service
                        ;(middleware/resource "public")
                        ;(route/method-param :_method)
                        ;; The routes are static and compiled at startup.
                        ;; We'll be using the MapTree router...
                        (route/router (route/expand-routes routes))]
   ::http/join? false
   ;; Normally we'd use Pedestal's Servlet hook on Jetty,
   ;; using the `::http/type :jetty` setting, but we want to use
   ;; our the chain-provider optimized for our application...

   ;; Call directly into Jetty, skipping Pedestal's Jetty Provider/options
   ::http/type jetty-server-fn
   ::http/chain-provider direct-jetty-provider


   ::http/port (or (some-> (System/getenv "PORT")
                           Integer/parseInt)
                   8080)
   ;; Our provider doesn't recognize these
   ;::http/container-options {:h2c? true
   ;                          :h2? false
   ;                          ;:keystore "test/hp/keystore.jks"
   ;                          ;:key-password "password"
   ;                          ;:ssl-port 8443
   ;                          :ssl? false
   ;                          ;:context-configurator #(jetty-util/add-servlet-filter % {:filter DoSFilter})
   ;                          }
   })

(defn -main [& args]
  (println "Starting your server...")
  (-> service
      (assoc ::http/join? true)
      http/create-server
      http/start))

(defn run-dev []
  ;; TODO: We could add back all the dev interceptors here if we wanted
  (-> service
      http/create-server
      http/start))

