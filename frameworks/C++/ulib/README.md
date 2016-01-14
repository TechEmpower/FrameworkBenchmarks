#ULib Benchmarking Test

This is the [ULib](http://stefanocasazza.github.io/ULib/) portion of a [benchmarking test suite](https://github.com/TechEmpower/FrameworkBenchmarks) comparing a variety of web development platforms.

### JSON Encoding Test

* [JSON test source](src/json.usp)

### Data-Store/Database Mapping Test

* [Database test source (SQL)](src/db.usp)
* [Database test source (REDIS)](src/rdb.usp)
* [Database test source (MONGODB)](src/mdb.usp)

### Variable Query Test

* [Variable Query test source (SQL)](src/query.usp)
* [Variable Query test source (REDIS)](src/rquery.usp)
* [Variable Query test source (MONGODB)](src/mquery.usp)

### Fortune Query Test

* [Fortune Query test source (SQL)](src/fortune.usp)
* [Fortune Query test source (REDIS)](src/rfortune.usp)
* [Fortune Query test source (MONGODB)](src/mfortune.usp)

### Variable Query (update) Test

* [Variable Query (update) test source (SQL)](src/update.usp)
* [Variable Query (update) test source (REDIS)](src/rupdate.usp)
* [Variable Query (update) test source (MONGODB)](src/mupdate.usp)

### Plaintext Test

* [Plaintext test source](src/plaintext.usp)

## Infrastructure Software Versions
The tests were run with:

* [ULib Version 1.4.2](https://github.com/stefanocasazza/ULib/archive/v1.4.2.tar.gz)

Output
======

[/json](http://www.techempower.com/benchmarks/#section=json)
-----
```
HTTP/1.1 200 OK
Date: Thu, 03 Jul 2014 10:11:10 GMT
Server: ULib 
Content-Length: 27
Content-Type: application/json

{"message":"Hello, World!"}
```

[/db](http://www.techempower.com/benchmarks/#section=db)
---
```
HTTP/1.1 200 OK
Date: Thu, 03 Jul 2014 10:14:51 GMT
Server: ULib 
Content-Length: 31
Content-Type: application/json

{"id":6227,"randomNumber":8489}
```

[/query?queries=10](http://www.techempower.com/benchmarks/#section=query)
-------------------
```
HTTP/1.1 200 OK
Date: Thu, 03 Jul 2014 10:14:51 GMT
Server: ULib 
Content-Length: 320
Content-Type: application/json

[{"id":6851,"randomNumber":7598},{"id":3968,"randomNumber":7325},{"id":8159,"randomNumber":348},{"id":9560,"randomNumber":7333},{"id":9938,"randomNumber":9080},{"id":1598,"randomNumber":1623},{"id":3280,"randomNumber":8707},{"id":4521,"randomNumber":6063},{"id":8173,"randomNumber":3690},{"id":3648,"randomNumber":8803}]
```

[/fortune](http://www.techempower.com/benchmarks/#section=fortune)
---------
```
HTTP/1.1 200 OK
Date: Thu, 03 Jul 2014 10:14:51 GMT
Server: ULib 
Content-Type: text/html; charset=UTF-8
Content-Length: 1227

<!doctype html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr><tr><td>11</td><td>&lt;script&gt;alert(&quot;This should not be displayed in a browser alert box.&quot;);&lt;/script&gt;</td></tr><tr><td>4</td><td>A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1</td></tr><tr><td>5</td><td>A computer program does what you tell it to do, not what you want it to do.</td></tr><tr><td>2</td><td>A computer scientist is someone who fixes things that aren&apos;t broken.</td></tr><tr><td>8</td><td>A list is only as strong as its weakest link. — Donald Knuth</td></tr><tr><td>0</td><td>Additional fortune added at request time.</td></tr><tr><td>3</td><td>After enough decimal places, nobody gives a damn.</td></tr><tr><td>7</td><td>Any program that runs right is obsolete.</td></tr><tr><td>10</td><td>Computers make very fast, very accurate mistakes.</td></tr><tr><td>6</td><td>Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen</td></tr><tr><td>9</td><td>Feature: A bug with seniority.</td></tr><tr><td>1</td><td>fortune: No such file or directory</td></tr><tr><td>12</td><td>フレームワークのベンチマーク</td></tr></table></body></html>
```

[/update?queries=10](http://www.techempower.com/benchmarks/#section=update)
-------------------
```
HTTP/1.1 200 OK
Date: Thu, 03 Jul 2014 10:14:51 GMT
Server: ULib 
Content-Length: 319
Content-Type: application/json

[{"id":7171,"randomNumber":351},{"id":6019,"randomNumber":9725},{"id":8118,"randomNumber":4023},{"id":7965,"randomNumber":1388},{"id":7797,"randomNumber":2249},{"id":112,"randomNumber":1108},{"id":6127,"randomNumber":4323},{"id":2597,"randomNumber":7509},{"id":2978,"randomNumber":7883},{"id":1111,"randomNumber":2228}]
```

[/plaintext](http://www.techempower.com/benchmarks/#section=plaintext)
----------
```
HTTP/1.1 200 OK
Date: Thu, 03 Jul 2014 10:14:51 GMT
Server: ULib 
Content-Type: text/plain
Content-Length: 13

Hello, World!
```
