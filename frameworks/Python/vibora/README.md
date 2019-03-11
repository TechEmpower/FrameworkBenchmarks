# [Vibora](https://github.com/vibora-io/vibora) Benchmark Test

The information below is specific to Vibora. For further guidance, 
review the [documentation](https://github.com/TechEmpower/FrameworkBenchmarks/wiki). 
Also note that there is additional information that's provided in 
the [Python README](../).

This is the Python Vibora portion of a [benchmarking tests suite](../../) 
comparing a variety of frameworks.

All test implementations are located within a single file 
([app.py](app.py)).

## Description

Vibora, Vibora + psycopg2

### Database

Postgres

## Test URLs
### JSON Encoding 

http://localhost:8080/json

### Single Row Random Query

http://localhost:8080/db

### Plaintext

http://localhost:8080/plaintext



The following tests cannot be currently run due to an issue with the framework
[Details Here] = https://github.com/vibora-io/vibora/issues/223

### Update random rows

http://localhost:8080/updates/?queries=

### Variable Row Query Test 

http://localhost:8080/db?queries=
