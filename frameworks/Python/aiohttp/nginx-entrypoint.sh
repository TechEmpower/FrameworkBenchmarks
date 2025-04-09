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

# Generate Nginx configuration
cat > /etc/nginx/conf.d/default.conf <<EOF

upstream aiohttp {
    least_conn;
EOF
i=0
while [ $i -lt $CORES ]; do
  echo "    server 127.0.0.1:$((8000 + i));" >> /etc/nginx/conf.d/default.conf
  i=$((i + 1))
done
cat >> /etc/nginx/conf.d/default.conf <<EOF
    keepalive 32;
}

server {
    listen 8080;

    access_log off;
    error_log /var/log/nginx/error.log crit;

    location / {
        proxy_pass http://aiohttp;
        proxy_http_version 1.1;
        proxy_set_header Connection "";
        proxy_redirect off;
        proxy_buffering off;
    }
}
EOF

echo "Starting Nginx..."
nginx -g "daemon off;"