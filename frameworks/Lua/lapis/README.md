# Lapis (Nginx + Lua(jit + Lapis)  Benchmark Test

The lua app is inside [web.lua](web.lua) which is compiled using [moonscript compiler](http://moonscript.org) from [web.moon](web.moon)
The nginx conf is inside [nginx.conf](nginx.conf)
Requires a nginx compiled with ngx_lua module, see [openresty.org](http://openresty.org), and ngxpostgres which is bundled with openresty.
Requires [lapis framework](http://leafo.net/lapis) installed
    
    luarocks install lapis

Requires postgresql hostname specified as IP address, if not possible then add resolver conf to nginx.conf.

The lapis benchmark is using its ORM, and no raw queries.


## Test URLs
### JSON Encoding 

http://localhost:8080/

### Single Row Random Query

http://localhost:8080/db

### Variable Row Query Test

http://localhost:8080/db?queries=2

### Fortune URL

http://localhost:8080/fortunes

### DB Updates URL

http://localhost:8080/update?queries=2

### Plaintext URL

http://localhost:8080/plaintext
