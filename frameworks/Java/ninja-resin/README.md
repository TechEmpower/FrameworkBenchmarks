# Ninja Framework Benchmarking Test

This is the [Ninja](http://www.ninjaframework.org/) portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

## IMPORTANT!!!!

ninja-resin is in essence just a copy of ninja-standalone with different datasources.
That means all stuff you change in ninja-resin should also be applied to 
ninja-standalone and vice-versa.

## Contributors and thanks

 * The whole TE team for making this possible. Thanks for your time and patience!
 * kpacha - Kudos to kpacha for creating the initial TE benchmark suite for Ninja!
 * raphael - Updating and maintenance of Ninja TE benchmarks
 * martin-g - Many thanks for fixing the issue with resin persistence.xml. Awesome! 


## Test URLs
### JSON Encoding Test

http://localhost:8080/ninja/json

### Single Query Test

http://localhost:8080/ninja/db

### Multiple Queries Test

http://localhost:8080/ninja/queries?5

### Fortunes Test

http://localhost:8080/ninja/fotunes

### Update Test

http://localhost:8080/ninja/update?5

### Plaintext Test

http://localhost:8080/ninja/plaintext


