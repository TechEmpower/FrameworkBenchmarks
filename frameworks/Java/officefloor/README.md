# OfficeFloor Benchmarking Test

OfficeFloor is graphically configured through its true inversion of control.

> Inversion of Control = Dependency Injection + Continuation Injection + Thread Injection

More information can be found at [http://officefloor.net](http://officefloor.net)

Further to this, graphical configuration is used.  The configuration is the following:

![Graphical Configuration](configuration.png "OfficeFloor graphical configuration")

The source code is:

* [JSON, PLAINTEXT, DB, QUERY, UPDATE](src/woof_benchmark/src/main/java/net/officefloor/benchmark/Logic.java)
* [FORTUNES](src/woof_benchmark/src/main/java/net/officefloor/benchmark/FortunesLogic.java)

### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/query?queries=

### UPDATE

http://localhost:8080/update?queries=

### FORTUNES

http://localhost:8080/fortunes