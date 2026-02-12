#!/bin/bash
set -e

BASE="http://tfb-database:6745"

# Wait for SoliDB to be ready
for i in $(seq 1 60); do
    if curl -sf "$BASE/_api/health" > /dev/null 2>&1; then
        break
    fi
    sleep 1
done

# Authenticate and get JWT token
TOKEN=$(curl -sf -X POST "$BASE/auth/login" \
    -H "Content-Type: application/json" \
    -d '{"username": "admin", "password": "benchmarkdbpass"}' \
    | sed -n 's/.*"token":"\([^"]*\)".*/\1/p')

# Create an API key
API_KEY=$(curl -sf -X POST "$BASE/_api/auth/api-keys" \
    -H "Content-Type: application/json" \
    -H "Authorization: Bearer $TOKEN" \
    -d '{"name": "soli-benchmark"}' \
    | sed -n 's/.*"key":"\([^"]*\)".*/\1/p')

# Write .env file for the soli framework
cat > /app/.env <<EOF
SOLIDB_HOST=$BASE
SOLIDB_DATABASE=hello_world
SOLIDB_API_KEY=$API_KEY
EOF

exec soli serve /app --port 3000
