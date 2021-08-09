# An experimental http library.

## Description.

An alternative http library inspired by actix and hyper. Implementation is rewritten with similar style and types.

## Database

PostgreSQL.

## Test URLs

### Test 1: JSON Encoding

    http://localhost:8080/json

### Test 2: Single Row Query

    http://localhost:8080/db

### Test 3: Multi Row Query

    http://localhost:8080/queries?q={count}

### Test 4: Fortunes (Template rendering)

    http://localhost:8080/fortune

### Test 5: Update Query

    http://localhost:8080/updates?q={count}

### Test 6: Plaintext

    http://localhost:8080/plaintext
