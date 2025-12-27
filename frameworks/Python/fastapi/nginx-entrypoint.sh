#!/bin/sh
set -e
gunicorn app:app -k uvicorn.workers.UvicornWorker -c fastapi_conf.py

cat > /fastapi/nginx.conf <<EOF
user www-data;
worker_processes auto;

events {
    worker_connections 65535;
}

http {
    keepalive_requests 100000;

    upstream fastapi {
        least_conn;
EOF

cat >> /fastapi/nginx.conf <<EOF
        keepalive 120;
    }

    server {
        listen 8080 reuseport;

        access_log off;
        error_log stderr error;

        location / {
            proxy_pass http://0.0.0.0:8080;
            proxy_http_version 1.1;
            proxy_set_header Connection "";
            proxy_redirect off;
            proxy_buffering off;
        }
    }
}
EOF

echo "Starting Nginx..."
nginx -c /fastapi/nginx.conf -g "daemon off;"