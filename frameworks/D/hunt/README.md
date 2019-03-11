# Hunt Benchmarking Test

This is the Hunt portion of a [benchmarking test suite](../) comparing a variety of web development platforms.


## Requirements
* Dlang > 2.077

## Test URLs
    
### PlanText Test

    http://localhost:8080/plaintext

### JSON Encoding Test

    http://localhost:8080/json
    
### Single database query

    http://localhost:8080/db
    
### Multiple database queries

    http://localhost:8080//queries?queries=10
    
### Database updates

    http://localhost:8080/updates?queries=10
