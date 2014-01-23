# ASP.NET MVC on Windows in asynchronous mode

This benchmark suite is using the new asynchronous features of .NET 4.5 and MVC, and also a little known JSON serializer [Jil](https://github.com/kevin-montrose/Jil).

## Optimizations

* JSON serialization done with Jil
* Database tests are done asynchronously (including at the ADO.NET level)
* Database update test is done in batch (by concatenating all UPDATE statements in a single query)
* Web site is precompiled and using a single assembly