
# Hexagon Benchmarking Test

This is the Hexagon portion of a [benchmarking test suite](../) comparing a variety of web
development platforms. The test utilizes Hexagon routes, serialization and database access.


## Local setup

    tar -Jxvf db.txz && \
    mongorestore dump/ && \
    rm -rf dump


## Tests

* [Hexagon application](/src/main/java/co/there4/hexagon/Benchmark.kt)


## Infrastructure Software Versions

* [Hexagon 0.3.2](http://there4.co/hexagon)


## Test URLs

* JSON Encoding Test: http://localhost:5050/json
* Data-Store/Database Mapping Test: http://localhost:5050/db?queries=5 
* Plain Text Test: http://localhost:5050/plaintext 
* Fortunes: http://localhost:5050/fortune 
* Database updates: http://localhost:5050/update

## Run on OpenShift

https://blog.openshift.com/run-gradle-builds-on-openshift/


## Copy to TFB

    rm -f db.txz
    
## Run inside vagrant

    toolset/run-tests.py --install server --mode verify --test hexagon
    
## Clear
    

## TODO

* Remove `benchmark_config` optional parameters. Check:
  //frameworkbenchmarks.readthedocs.io/en/latest/Codebase/Framework-Files/#benchmark-config-file

* Document common commands to test the framework inside Vagrant's development machine

