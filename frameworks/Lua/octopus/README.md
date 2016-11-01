# Octopus (Nginx + LuaJIT)  Benchmark Test

The test lua app is inside [app](app) directory.
The configuration is in [config.lua](config.lua).
Clones, compiles and runs nginx with ngx_lua module, see [openresty.org](http://openresty.org).
Prerequisites: ```libssl-dev gcc g++ build-essential```.
Requires [git](https://git-scm.com).
Requires postgresql hostname specified as IP address, if not possible then add resolver conf to [config.lua](config.lua).
The Octopus benchmark is using its ORM, and no raw queries.


## Test URLs
### Plaintext URL

http://localhost:8080/plaintext

### JSON Encoding 

http://localhost:8080/json

### Single Row Random Query

http://localhost:8080/db

### Multiple Row Query Test

http://localhost:8080/queries?queries=2

### Fortune URL

http://localhost:8080/fortunes

### DB Updates URL

http://localhost:8080/update?queries=2
