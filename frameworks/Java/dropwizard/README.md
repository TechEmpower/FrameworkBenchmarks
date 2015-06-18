# Dropwizard Benchmarking Test

This project consists of 3 tests:

* MySql and Postgres - uses: `HelloWorldService.java` and `HelloWorldConfiguration.java`
* MongoDB - uses: `HelloMongoService.java` and `HelloMongoConfiguration.java`

All other `resources` and `model` code are shared. The main `*Service.java` is set based on maven profiles.
Respective `.yml` configuration files are specified in each `setup_*.sh` script.