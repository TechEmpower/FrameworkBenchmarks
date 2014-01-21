# ASP.NET MVC on Windows in asynchronous mode

This benchmark suite is using the new asynchronous features of .NET 4.0 and MVC4, and also a little known JSON serializer.

## Optimizations

* JSON serialization done with [Jil](https://github.com/kevin-montrose/Jil)
* Database tests are done asynchronously (including at the ADO.NET level)
* Database update test is done in batch (by concatenating all UPDATE statements in a single query)
* Web site is precompiled and using a single assembly