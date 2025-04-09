#!/bin/sh
set -e
CORES=$(nproc)
echo "$CORES cores detected, starting $CORES aiohttp workers..."

for i in $(seq 0 $((CORES-1))); do
  PORT=$((8000 + i))
  echo "Starting worker on port $PORT"
  python3 -O -m app.app --port $PORT &
done

echo "Waiting for all workers to be ready..."
for i in $(seq 0 $((CORES-1))); do
  PORT=$((8000 + i))
  until nc -z localhost $PORT; do
    echo "Waiting for port $PORT..."
    sleep 0.1
  done
done

cat > /aiohttp/nginx.conf <<EOF
worker_processes auto;

events {
    worker_connections 65535;
}

http {
    keepalive_requests 10000000;

    upstream aiohttp {
        least_conn;
EOF

for i in $(seq 0 $((CORES-1))); do
  echo "        server 127.0.0.1:$((8000 + i));" >> /aiohttp/nginx.conf
done

cat >> /aiohttp/nginx.conf <<EOF
        keepalive 32;
    }

    server {
        listen 8080 reuseport;

        access_log off;
        error_log stderr error;

        location / {
            proxy_pass http://aiohttp;
            proxy_http_version 1.1;
            proxy_set_header Connection "";
            proxy_redirect off;
            proxy_buffering off;
        }
    }
}
EOF

echo "Starting Nginx..."
nginx -c /aiohttp/nginx.conf -g "daemon off;"