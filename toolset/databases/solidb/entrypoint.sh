#!/bin/bash
set -e

solidb &
SOLIDB_PID=$!

BASE="http://localhost:6745"

# Wait for SoliDB to be ready
for i in $(seq 1 30); do
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

AUTH="Authorization: Bearer $TOKEN"

# Create database
curl -sf -X POST "$BASE/_api/database" \
    -H "Content-Type: application/json" \
    -H "$AUTH" \
    -d '{"name": "hello_world"}' || true

DB="$BASE/_api/database/hello_world"

# Create collections before inserting documents
curl -sf -X POST "$DB/collection" \
    -H "Content-Type: application/json" \
    -H "$AUTH" \
    -d '{"name": "fortunes"}' || true

curl -sf -X POST "$DB/collection" \
    -H "Content-Type: application/json" \
    -H "$AUTH" \
    -d '{"name": "worlds"}' || true

# Seed fortune collection via batch insert
curl -sf -X POST "$DB/document/fortunes/_batch" \
    -H "Content-Type: application/json" \
    -H "$AUTH" \
    -d '[
        {"id": 1, "message": "fortune: No such file or directory"},
        {"id": 2, "message": "A computer scientist is someone who fixes things that aren'\''t broken."},
        {"id": 3, "message": "After enough decimal places, nobody gives a damn."},
        {"id": 4, "message": "A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1"},
        {"id": 5, "message": "A computer program does what you tell it to do, not what you want it to do."},
        {"id": 6, "message": "Emacs is a nice operating system, but I prefer UNIX. \u2014 Tom Christaensen"},
        {"id": 7, "message": "Any program that runs right is obsolete."},
        {"id": 8, "message": "A list is only as strong as its weakest link. \u2014 Donald Knuth"},
        {"id": 9, "message": "Feature: A bug with seniority."},
        {"id": 10, "message": "Computers make very fast, very accurate mistakes."},
        {"id": 11, "message": "<script>alert(\"This should not be displayed in a browser alert box.\");<\/script>"},
        {"id": 12, "message": "\u30d5\u30ec\u30fc\u30e0\u30ef\u30fc\u30af\u306e\u30d9\u30f3\u30c1\u30de\u30fc\u30af"}
    ]'

# Seed world collection via batch inserts (1000 docs per batch)
for batch in $(seq 0 9); do
    start=$((batch * 1000 + 1))
    end=$((batch * 1000 + 1000))
    docs="["
    for i in $(seq $start $end); do
        rnd=$(( (RANDOM % 10000) + 1 ))
        if [ $i -gt $start ]; then docs="$docs,"; fi
        docs="$docs{\"id\":$i,\"randomNumber\":$rnd}"
    done
    docs="$docs]"
    curl -sf -X POST "$DB/document/worlds/_batch" \
        -H "Content-Type: application/json" \
        -H "$AUTH" \
        -d "$docs"
done

echo "SoliDB seeded successfully"

wait $SOLIDB_PID
