# Lwan

This is the configuration files to benchmark the [Lwan](http://lwan.ws/?src=twfb)
web server/framework.

## FAQ

### Where's the source code for the benchmark?

Source code for the benchmark itself is currently living in [in Lwan's
repository](https://github.com/lpereira/lwan/tree/master/techempower) for
practical reasons, but will move to [this repository right after Round 10 is
complete](https://github.com/TechEmpower/FrameworkBenchmarks/issues/1372).

In any case, the script to install the framework in the test machines [pulls
a specific snapshot of the Lwan source
code](https://github.com/lpereira/lwan/tree/49607addb31879e2aa2b701317773674662315aa),
including the benchmark code; and, due to the nature of Git, it's not
possible to alter that code without changing which commit hash to checkout
in this repository.

### The JSON test RPS value seems ridiculous; is Lwan cheating?

Come on. There's no optimization without cheating. There's a somewhat
[detailed blog post about
this](http://tia.mat.br/blog/html/2014/10/06/life_of_a_http_request.html?src=twfb).
However, that level of cheating is considered by Lwan's author to be fair
game, since it's not related to the benchmark code itself.

In fact, the serialization library has been picked solely because it was
available in the [CCAN](http://ccodearchive.net/).  It's written by [Joseph
A.  Adams](http://ccodearchive.net/info/json.html), and has been slightly
tweaked to fix some compiler warnings and a few other minor changes.  No
major optimization or trickery has been made there: it's essentially the
same code.  Props to him for a clean code that performs well.

It's all open source anyway, feel free to go ahead and inspect the code.

### The database results aren't that good. Why?

The database tests are included for completeness sake. The database
abstraction layer (which works with SQLite and MySQL) was written
specifically for this benchmark, and is completely synchronous: while
one query is being performed, all other clients wait.

So the uninspiring results are expected. Database connectivity is
important and will eventually be revisited.

### I've seen the preliminary results and Lwan didn't fare well in the plaintext benchmark. Is there a reason why?

These benchmarks are performed using pipelined requests. For practical
reasons (almost no client supports them, at least by default), this was not
implemented in Lwan: the server was dropping the request buffer and waiting
for the next request to come in, while `wrk` was waiting for responses to
come out, eventually timeouting and sending more pipelined requests, only to
repeat, until the test actually ended.

Pipelining has been implemented in a hurry to get included in Round 10.

### The results are pretty good, can I use this in my next web app?

Writing web apps in C? Only if you're Crazy. (The uppercase C is not a typo.)

Seriously, Lwan is just a toy.  It's an experiment.  Most things you'll need
from real frameworks won't be available.  You'll have a hard time trying to do
the simplest things.  There are far more important things to look for while
picking a framework, and performance, while important, is not the most
important factor.  However, if you're feeling adventurous, know that Lwan is
free software, and is very open to pull requests.

### I have other questions, is there a FAQ somewhere else?

Sure there is. [See Lwan's web page](http://lwan.ws/?src=twfb) or contact the author
directly.

## Requirements

GCC, SQLite 3.7, MySQL/MariaDB client libraries, Lua 5.1/LuaJit 2.0, Linux 3.0,
optionally jemalloc or tcmalloc.

## Tests available

### 1. JSON serialization

URL: /json

### 2. Single database query

URL: /db

### 3. Multiple database queries

URL: /queries?queries=N

### 4. Fortunes

URL: /fortunes

### 6. Plaintext

URL: /plaintext

## Contact

[Leandro Pereira](http://tia.mat.br/?src=twfb) <[leandro@tia.mat.br](mailto:leandro@tia.mat.br)>
