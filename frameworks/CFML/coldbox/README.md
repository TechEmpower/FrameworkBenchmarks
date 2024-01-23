# CFML ColdBox Benchmarking Test

The default test runs on Lucee Server.  The Adobe version runs on Adobe ColdFusion. 
The code is the same between both CF engines.

### Test Type Implementation Source Code

* [JSON](src/handlers/Main.cfc)
* [PLAINTEXT](src/handlers/Main.cfc)
* [DB](src/handlers/Main.cfc)
* [QUERY](src/handlers/Main.cfc)
* [CACHED QUERY](src/handlers/Main.cfc)
* [UPDATE](src/handlers/Main.cfc)
* [FORTUNES](src/handlers/Main.cfc)

## Important Libraries
The tests were run with:
* [CommandBox](https://commandbox.ortusbooks.com/)
* [ortussolutions/commandbox Docker image](https://hub.docker.com/r/ortussolutions/commandbox/)
* [Lucee Server](https://www.lucee.org/)
* [Adobe ColdFusion](https://coldfusion.adobe.com/)
* [ColdBox MVC](https://www.coldbox.org/)

## Test URLs
### JSON

http://localhost:8080/index.cfm?event=main.json

### PLAINTEXT

http://localhost:8080/index.cfm?event=main.plaintext

### DB

http://localhost:8080/index.cfm?event=main.db

### QUERY

http://localhost:8080/index.cfm?event=main.query&queries=

### CACHED QUERY

http://localhost:8080/index.cfm?event=main.cached_query&queries=

### UPDATE

http://localhost:8080/index.cfm?event=main.update&queries=

### FORTUNES

http://localhost:8080/index.cfm?event=main.fortunes
