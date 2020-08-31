daemon off;
worker_processes $NGINX_WORKER_PROCESES;
worker_cpu_affinity auto;
timer_resolution 1s;
error_log stderr error;

events {
  multi_accept on;
  worker_connections 65535;
}

http {
  access_log    off;
  server_tokens off;
  msie_padding  off;

  sendfile    on;
  tcp_nopush  on;
  tcp_nodelay on;

  keepalive_timeout  65;
  keepalive_disable  none;
  keepalive_requests 1000000;

  include /racket/config/upstream.conf;

  server {
    listen 8080 default_server reuseport deferred backlog=65535 fastopen=4096;

    location / {
      proxy_pass            http://app;
      proxy_http_version    1.1;
      proxy_set_header      Connection "";
    }
  }
}