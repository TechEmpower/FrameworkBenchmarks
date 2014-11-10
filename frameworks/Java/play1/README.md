Play 1.x
========

This is a benchmark application for Play 1.2.x and 1.3.x.

This application implements `Round 8` test cases and requires `java 1.6+`.

The application uses the [Rythm template engine](https://github.com/greenlaw110/play-rythm) and Hibernate persistence (3.6.10 for Play 1.2.x and 4.1.3 for Play 1.3.x).

Installation
============
Install Mysql 5.6+

Create the database with the following (excerpt from [create.sql](https://github.com/TechEmpower/FrameworkBenchmarks/blob/master/config/create.sql))
```sql
# create benchmark user
GRANT ALL ON *.* TO 'benchmarkdbuser'@'%' IDENTIFIED BY 'benchmarkdbpass';
GRANT ALL ON *.* TO 'benchmarkdbuser'@'localhost' IDENTIFIED BY 'benchmarkdbpass';

DROP DATABASE IF EXISTS hello_world;
CREATE DATABASE hello_world;
USE hello_world;
```

Download [Play](http://www.playframework.com/download)
Add the path to the `PATH` variable

Run the application
`play run`

Insert the data: http://localhost:8080/setup

Launch
======

Check in `conf/application.conf` that the following line is present: 
```
application.mode=prod
```
Precompile the application 
```
play precompile
```
Launch the application with the `-Dprecompiled=true` option via 
```
play run -Dprecompiled=true
```
or 
```
play start -Dprecompiled=true
```

Extra urls
==========

For convenience, the application include the following actions: 
  - [index](http://localhost:8080/): a help page that lists all the actions

Output
======

[/json](http://www.techempower.com/benchmarks/#section=json)
-----
```
< HTTP/1.1 200 OK
< Server: Play! Framework;1.3.x-1.3.0RC1;dev
< Content-Type: application/json; charset=utf-8
< Date: Thu, 03 Jul 2014 10:11:10 GMT
< Set-Cookie: PLAY_FLASH=; Expires=Thu, 03 Jul 2014 10:11:10 GMT; Path=/
< Set-Cookie: PLAY_ERRORS=; Expires=Thu, 03 Jul 2014 10:11:10 GMT; Path=/
< Set-Cookie: PLAY_SESSION=; Expires=Thu, 03 Jul 2014 10:11:10 GMT; Path=/
< Cache-Control: no-cache
< Content-Length: 26

{"message":"Hello World!"}
```

[/db](http://www.techempower.com/benchmarks/#section=db)
---
```
< HTTP/1.1 200 OK
< Server: Play! Framework;1.3.x-1.3.0RC1;dev
< Content-Type: application/json; charset=utf-8
< Date: Thu, 03 Jul 2014 10:14:51 GMT
< Set-Cookie: PLAY_FLASH=; Expires=Thu, 03 Jul 2014 10:14:51 GMT; Path=/
< Set-Cookie: PLAY_ERRORS=; Expires=Thu, 03 Jul 2014 10:14:51 GMT; Path=/
< Set-Cookie: PLAY_SESSION=; Expires=Thu, 03 Jul 2014 10:14:51 GMT; Path=/
< Cache-Control: no-cache
< Content-Length: 29
<
{"id":3833,"randomNumber":84}
```

[/queries?queries=10](http://www.techempower.com/benchmarks/#section=query)
-------------------
```
< HTTP/1.1 200 OK
< Server: Play! Framework;1.3.x-1.3.0RC1;dev
< Content-Type: application/json; charset=utf-8
< Date: Thu, 03 Jul 2014 10:15:53 GMT
< Set-Cookie: PLAY_FLASH=; Expires=Thu, 03 Jul 2014 10:15:53 GMT; Path=/
< Set-Cookie: PLAY_ERRORS=; Expires=Thu, 03 Jul 2014 10:15:53 GMT; Path=/
< Set-Cookie: PLAY_SESSION=; Expires=Thu, 03 Jul 2014 10:15:53 GMT; Path=/
< Cache-Control: no-cache
< Content-Length: 320
<
[{"id":6851,"randomNumber":7598},{"id":3968,"randomNumber":7325},
{"id":8159,"randomNumber":348},{"id":9560,"randomNumber":7333},
{"id":9938,"randomNumber":9080},{"id":1598,"randomNumber":1623},{"id":3280,"randomNumber":8707},
"id":4521,"randomNumber":6063},{"id":8173,"randomNumber":3690},{"id":3648,"randomNumber":8803}]
```

[/fortunes](http://www.techempower.com/benchmarks/#section=fortune)
---------
```
< HTTP/1.1 200 OK
< Server: Play! Framework;1.3.x-1.3.0RC1;dev
< Content-Type: text/html; charset=utf-8
< Date: Thu, 03 Jul 2014 10:20:50 GMT
< Set-Cookie: PLAY_FLASH=; Expires=Thu, 03 Jul 2014 10:20:50 GMT; Path=/
< Set-Cookie: PLAY_ERRORS=; Expires=Thu, 03 Jul 2014 10:20:50 GMT; Path=/
< Set-Cookie: PLAY_SESSION=; Expires=Thu, 03 Jul 2014 10:20:50 GMT; Path=/
< Cache-Control: no-cache
< Content-Length: 8633
<
<!DOCTYPE html>
<html>
	<head>
		<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
		<title>Fortunes</title>
	</head>
	<body>
		<table>
			<tr>
					<th>id</th>
					<th>message</th>
			</tr>
			<tr>
					<td>10</td>
					<td>&lt;script&gt;alert(&quot;This should not be displayed in a browser alert box.&quot;);&lt;/script&gt;</td>
			</tr>
			<tr>
					<td>3</td>
					<td>A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1</td>
			</tr>
			<tr>
					<td>4</td>
					<td>A computer program does what you tell it to do, not what you want it to do.</td>
			</tr>
			<tr>
					<td>1</td>
					<td>A computer scientist is someone who fixes things that aren&apos;&apos;t broken.</td>
			</tr>
			...
```
[/updates?queries=10](http://www.techempower.com/benchmarks/#section=update)
-------------------
```
< HTTP/1.1 200 OK
< Server: Play! Framework;1.3.x-1.3.0RC1;dev
< Content-Type: application/json; charset=utf-8
< Date: Thu, 03 Jul 2014 10:24:16 GMT
< Set-Cookie: PLAY_FLASH=; Expires=Thu, 03 Jul 2014 10:24:16 GMT; Path=/
< Set-Cookie: PLAY_ERRORS=; Expires=Thu, 03 Jul 2014 10:24:16 GMT; Path=/
< Set-Cookie: PLAY_SESSION=; Expires=Thu, 03 Jul 2014 10:24:16 GMT; Path=/
< Cache-Control: no-cache
< Content-Length: 319
<
[{"id":7171,"randomNumber":351},{"id":6019,"randomNumber":9725},
{"id":8118,"randomNumber":4023},{"id":7965,"randomNumber":1388},
{"id":7797,"randomNumber":2249},{"id":112,"randomNumber":1108},{"id":6127,"randomNumber":4323},
{"id":2597,"randomNumber":7509},{"id":2978,"randomNumber":7883},{"id":1111,"randomNumber":2228}]
```

[/plaintext](http://www.techempower.com/benchmarks/#section=plaintext)
----------
```
< HTTP/1.1 200 OK
< Server: Play! Framework;1.3.x-1.3.0RC1;dev
< Content-Type: text/plain; charset=utf-8
< Date: Thu, 03 Jul 2014 10:25:25 GMT
< Set-Cookie: PLAY_FLASH=; Expires=Thu, 03 Jul 2014 10:25:25 GMT; Path=/
< Set-Cookie: PLAY_ERRORS=; Expires=Thu, 03 Jul 2014 10:25:25 GMT; Path=/
< Set-Cookie: PLAY_SESSION=; Expires=Thu, 03 Jul 2014 10:25:25 GMT; Path=/
< Cache-Control: no-cache
< Content-Length: 11
<
hello world
```
