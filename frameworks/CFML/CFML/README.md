# CFML Lucee/Adobe Benchmarking Test

The default test runs on Lucee Server.  The Adobe version runs on Adobe ColdFusion. 
The code is the same between both CF engines.

### Test Type Implementation Source Code

* [JSON](src/json.cfm)
* [PLAINTEXT](src/plaintext.cfm)
* [DB](src/db.cfm)
* [QUERY](src/query.cfm)
* [CACHED QUERY](src/cached_query.cfm)
* [UPDATE](src/update.cfm)
* [FORTUNES](src/fortunes.cfm)

## Important Libraries
The tests were run with:
* [CommandBox](https://commandbox.ortusbooks.com/)
* [ortussolutions/commandbox Docker image](https://hub.docker.com/r/ortussolutions/commandbox/)
* [Lucee Server](https://www.lucee.org/)
* [Adobe ColdFusion](https://coldfusion.adobe.com/)

## Test URLs
### JSON

http://localhost:8080/json.cfm

### PLAINTEXT

http://localhost:8080/plaintext.cfm

### DB

http://localhost:8080/db.cfm

### QUERY

http://localhost:8080/query.cfm?queries=

### CACHED QUERY

http://localhost:8080/cached_query.cfm?queries=

### UPDATE

http://localhost:8080/update.cfm?queries=

### FORTUNES

http://localhost:8080/fortunes.cfm
