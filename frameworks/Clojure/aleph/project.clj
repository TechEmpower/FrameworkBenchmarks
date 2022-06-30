(defproject hello "aleph"
  :description "JSON/plaintext tests"
  :dependencies [[org.clojure/clojure "1.11.0"]
                 [aleph "0.4.7"]
                 [metosin/jsonista "0.3.5"]
                 [hiccup "1.0.5"]
                 [io.netty/netty-transport-native-epoll "4.1.65.Final" :classifier "linux-x86_64"]
                 [com.github.arnaudgeiser/porsas "0.0.1-alpha14"
                  :exclusions [io.netty/netty-codec-dns
                               io.netty/netty-codec
                               io.netty/netty-buffer
                               io.netty/netty-common
                               io.netty/netty-codec-http
                               io.netty/netty-codec-http2
                               io.netty/netty-codec-socks
                               io.netty/netty-handler
                               io.netty/netty-handler-proxy
                               io.netty/netty-transport
                               io.netty/netty-resolver-dns
                               io.netty/netty-resolver]]
                 [com.clojure-goes-fast/clj-async-profiler "0.5.1"]]
  :main hello.handler
  :jvm-opts ^:replace ["-Dclojure.compiler.direct-linking=true"]
  :aot :all)
