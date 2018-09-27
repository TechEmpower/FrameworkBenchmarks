TechEmpower Framework Benchmark - ActFramework Change log

**1.8.8h**

* fix broken testcases on JPA orm based tests

**1.8.8f**

* experiment: take HikariCP from eclipse-mysql permutations

**1.8.8e**

* update act to 1.8.8-RC12-SNAPSHOT
    - fix fortune mustache bad performance issue

**1.8.8d**

* update act to 1.8.8-RC11

**1.8.8c**

* revert max connection to 48 for eclipselink-pgsql and eclipselink-mysql update test
* revert max connection to 10 for hibernate-pgsql update test
* change max connection to 48 for ebean-xxx read tests

**1.8.8b**

* Use internal datasource pool to replace druid for Ebean permutations
* change max connection to 20 for all updates tests
* change max connection to 5 for ebean-pgsql and hibernate-pgsql updates tests
