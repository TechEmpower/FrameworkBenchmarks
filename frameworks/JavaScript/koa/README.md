# Koa Benchmarking Test

This is the Koa portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

Information about Koa can be found at https://github.com/koajs/koa

### JSON Encoding Test

* [JSON test source](app.js)

### Data-Store/Database Mapping Test

* [DB test controller/model](app.js)

## Resources
* http://nodejs.org/api/cluster.html

## Test URLs
### JSON Encoding Test

http://TFB-server:8080/json

### Data-Store/Database Mapping Test

MongoDB:
http://TFB-server:8080/mongoose/

MySQL:
http://TFB-server:8080/mysql-orm/

### Variable Query Test

MongoDB:
http://TFB-server:8080/mongoose/2

MySQL:
http://TFB-server:8080/mysql-orm/2

### Fortune Test

MySQL:
http://TFB-server:8080/fortune

### Database Update Test

MongoDB:
http://TFB-server:8080/mongoose-update/2

MySQL:
http://TFB-server:8080/mysql-orm-update/2

