#!/bin/sh
set -e
CORES=$(nproc)
echo "$CORES cores detected, starting $CORES aiohttp workers..."

for i in $(seq 0 $((CORES-1))); do
  SOCKET="/run/aiohttp-$i.sock"
  echo "Starting worker on socket $SOCKET"
  python3 -O -m app.app --socket $SOCKET &
done

echo "Waiting for all workers to be ready..."
for i in $(seq 0 $((CORES-1))); do
  SOCKET="/run/aiohttp-$i.sock"
  until [ -S "$SOCKET" ]; do
    echo "Waiting for socket $SOCKET..."
    sleep 0.2
  done
  chown root:www-data "$SOCKET"
  chmod 660 "$SOCKET"
done

cat > /aiohttp/nginx.conf <<EOF
user www-data;
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
  echo "        server unix:/run/aiohttp-$i.sock fail_timeout=0;" >> /aiohttp/nginx.conf
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