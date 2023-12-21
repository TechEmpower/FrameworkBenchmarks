Middleware
===
This directory contains your application middleware.  
Middleware let you define custom functions that can be run for a certain URL path groups.

Middleware have to be this interface
```nim
proc (c:Context, p:Params):Future[Response]
```
