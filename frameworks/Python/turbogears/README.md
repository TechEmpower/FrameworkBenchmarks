# TurboGears Benchmark Test 

Single file test, [app.py](app.py)

## Description

TurboGears framework (http://turbogears.org)

### Database

MySQL

### Server

* Gunicorn + Meinheld

## Test URLs
### JSON Encoding

http://localhost:8080/json

### Plaintext

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### Query

http://localhost:8080/queries?queries=2

### Update

http://localhost:8080/updates?queries=2

### Fortune

http://localhost:8080/fortune
