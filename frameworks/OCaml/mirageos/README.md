# MirageOS Unikernel Benchmarking Test

[MirageOS](https://github.com/mirage/mirage) is a unikernel implemented in OCaml. This project uses the 
[ocaml-cohttp](https://github.com/mirage/ocaml-cohttp) sub-project to implement a simple HTTP server.

### Test URLs

[/plaintext](http://www.techempower.com/benchmarks/#section=plaintext)
----------
```
HTTP/1.1 200 OK
Server: MirageOS
Content-Type: text/plain
Date: Mon, 01 Jan 1970 00:00:01 GMT
Content-Length: 13

Hello, world!
```


### TODO

* Under load the server will crash with a Unix.EINVAL or similar exception, this open issue is 
detailed [here](https://github.com/mirage/ocaml-cohttp/issues/503)