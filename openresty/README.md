# Openresty (Nginx + Lua(jit) Benchmark Test

The lua app is inside [app.lua](app.lua)
The nginx conf is inside [nginx.conf](nginx.conf)
Requires a nginx compiled with ngx_lua module, see [openresty.org](http://openresty.org)

Requires mysql hostname specified as IP address, if not possible then add resolver conf to nginx.conf.


## Test URLs
### JSON Encoding 

http://localhost:8080/json

### Single Row Random Query

http://localhost:8080/db

### Variable Row Query Test

http://localhost:8080/db?queries=2
