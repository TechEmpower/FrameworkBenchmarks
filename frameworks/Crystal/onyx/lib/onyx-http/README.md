<a href="https://onyxframework.org"><img width="100" height="100" src="https://onyxframework.org/img/logo.svg"></a>

# Onyx::HTTP

[![Built with Crystal](https://img.shields.io/badge/built%20with-crystal-000000.svg?style=flat-square)](https://crystal-lang.org/)
[![Travis CI build](https://img.shields.io/travis/onyxframework/http/master.svg?style=flat-square)](https://travis-ci.org/onyxframework/http)
[![API docs](https://img.shields.io/badge/api_docs-online-brightgreen.svg?style=flat-square)](https://api.onyxframework.org/http)
[![Latest release](https://img.shields.io/github/release/onyxframework/http.svg?style=flat-square)](https://github.com/onyxframework/http/releases)

An HTTP framework for [Crystal](https://crystal-lang.org).

## Supporters ❤️

Thanks to all my patrons, I can continue working on beautiful Open Source Software! 🙏

[Lauri Jutila](https://github.com/ljuti)

*You can become a patron too in exchange of prioritized support and other perks*

<a href="https://www.patreon.com/vladfaust"><img height="50" src="https://onyxframework.org/img/patreon-button.svg"></a>

## About 👋

Onyx::HTTP is a collection of [`HTTP::Handler`](https://crystal-lang.org/api/0.27.2/HTTP/Handler.html)'s to use in web [Crystal](https://crystal-lang.org/) applications. It also includes a convenient [`HTTP::Server`](https://crystal-lang.org/api/0.27.2/HTTP/Server.html) wrapper. Onyx::HTTP shard serves as a foundation for higher-level Onyx components, such as [Onyx::REST](https://github.com/onyxframework/rest), but it can definetely be used as a stand-alone library.

## Installation 📥

Add this to your application's `shard.yml`:

```yaml
dependencies:
  onyx-http:
    github: onyxframework/http
    version: ~> 0.1.0
```

This shard follows [Semantic Versioning v2.0.0](http://semver.org/), so check [releases](https://github.com/onyxframework/http/releases) and change the `version` accordingly. Please visit [github.com/crystal-lang/shards](https://github.com/crystal-lang/shards) to know more about Crystal shards.

## Usage 💻

Onyx::HTTP includes multiple useful [`HTTP::Handler`](https://crystal-lang.org/api/0.27.2/HTTP/Handler.html)s and an [`HTTP::Server`](https://crystal-lang.org/api/0.27.2/HTTP/Server.html) wrapper. If you don't know anything about the Crystal HTTP server, then check out its [official docs](https://crystal-lang.org/reference/overview/http_server.html) and [API](https://crystal-lang.org/api/0.27.2/HTTP/Server.html) before continuing.

### Simple server

Start with a simple [`Onyx::HTTP::Server`](https://api.onyxframework.org/Onyx/HTTP/Server.html) example:

```crystal
require "onyx-http"

server = Onyx::HTTP::Server.new do |env|
  env.response << "Hello Onyx"
end

server.bind_tcp(5000)
server.listen
```

```sh
> curl http://localhost:5000
Hello Onyx
```

![Screenshot #1](https://user-images.githubusercontent.com/7955682/52903116-5c4bd680-322a-11e9-9163-53edea029f89.png)

### Logging

We'd like to see the actual request in the STDOUT. Use [`Onyx::HTTP::Logger`](https://api.onyxframework.org/Onyx/HTTP/Logger.html) for that:

```crystal
require "onyx-http"

logger = Onyx::HTTP::Logger.new

server = Onyx::HTTP::Server.new(logger) do |env|
  env.response << "Hello Onyx"
end

server.bind_tcp(5000)
server.listen
```

```sh
> curl http://localhost:5000
Hello Onyx
```

![Screenshot #2](https://user-images.githubusercontent.com/7955682/52903118-5fdf5d80-322a-11e9-84b2-2658a0051d08.png)

### Request meta

It's a good idea to add an ID to the request and also a time elapsed to process it for further analysis. There are [`Onyx::HTTP::RequestID`](https://api.onyxframework.org/Onyx/HTTP/RequestID.html) and [`Onyx::HTTP::ResponseTime`](https://api.onyxframework.org/Onyx/HTTP/ResponseTime.html) handlers:

```crystal
require "onyx-http"

logger = Onyx::HTTP::Logger.new
request_id = Onyx::HTTP::RequestID.new
response_time = Onyx::HTTP::ResponseTime.new

server = Onyx::HTTP::Server.new(response_time, request_id, logger) do |env|
  env.response << "Hello Onyx"
end

server.bind_tcp(5000)
server.listen
```

```sh
> curl http://localhost:5000 -v
* Rebuilt URL to: http://localhost:5000/
*   Trying 127.0.0.1...
* Connected to localhost (127.0.0.1) port 5000 (#0)
> GET / HTTP/1.1
> Host: localhost:5000
> User-Agent: curl/7.47.0
> Accept: */*
>
< HTTP/1.1 200 OK
< Connection: keep-alive
< X-Request-ID: 14c0259b-c020-4c85-931c-556e1ff266da
< X-Response-Time: 120
< Content-Length: 10
<
* Connection #0 to host localhost left intact
Hello Onyx
```

![Screenshot #3](https://user-images.githubusercontent.com/7955682/52903120-6372e480-322a-11e9-9273-ca7bc478b32d.png)

### CORS

Modern APIs usually require proper [CORS](https://en.wikipedia.org/wiki/Cross-origin_resource_sharing) handling. It is achievable with [`Onyx::HTTP::CORS`](https://api.onyxframework.org/Onyx/HTTP/CORS.html):

```crystal
require "onyx-http"

logger = Onyx::HTTP::Logger.new
request_id = Onyx::HTTP::RequestID.new
response_time = Onyx::HTTP::ResponseTime.new
cors = Onyx::HTTP::CORS.new

server = Onyx::HTTP::Server.new(response_time, request_id, logger, cors) do |env|
  env.response << "Hello Onyx"
end

server.bind_tcp(5000)
server.listen

```

```sh
> curl http://localhost:5000 -v
* Rebuilt URL to: http://localhost:5000/
*   Trying 127.0.0.1...
* Connected to localhost (127.0.0.1) port 5000 (#0)
> GET / HTTP/1.1
> Host: localhost:5000
> User-Agent: curl/7.47.0
> Accept: */*
>
< HTTP/1.1 200 OK
< Connection: keep-alive
< X-Request-ID: af4ded9e-2f72-41aa-bac2-308b38198af6
< Access-Control-Allow-Origin: *
< X-Response-Time: 233
< Content-Length: 10
<
* Connection #0 to host localhost left intact
Hello Onyx
```

### Routing

Almost every web application requires routing. It is quite simple with [`Onyx::HTTP::Router`](https://api.onyxframework.org/Onyx/HTTP/Router.html):

```crystal
require "onyx-http"

logger = Onyx::HTTP::Logger.new
request_id = Onyx::HTTP::RequestID.new
response_time = Onyx::HTTP::ResponseTime.new

router = Onyx::HTTP::Router.new do
  get "/" do |env|
    env.response << "Hello Onyx"
  end

  post "/echo" do |env|
    env.response << env.request.body.try &.gets_to_end
  end
end

server = Onyx::HTTP::Server.new(response_time, request_id, logger, router)

server.bind_tcp(5000)
server.listen
```

```sh
> curl http://localhost:5000
Hello Onyx
> curl -X POST -d "Knock-knock" http://localhost:5000/echo
Knock-knock
```

![Screenshot #4](https://user-images.githubusercontent.com/7955682/52903121-68379880-322a-11e9-9d45-0517d9e29dda.png)

### Rescuing

By default, an unhandled exception would halt the request processing, put the error backtrace into `STDERR` and print `500 Internal Server Error` into the response body. We can change this behaviour with [`Onyx::HTTP::Rescuer`](https://api.onyxframework.org/Onyx/HTTP/Rescuer.html).

This shard comes with [`Onyx::HTTP::Rescuers::Standard`](https://api.onyxframework.org/Onyx/HTTP/Rescuers/Standard.html), [`Onyx::HTTP::Rescuers::Silent`](https://api.onyxframework.org/Onyx/HTTP/Rescuers/Silent.html) and [`Onyx::HTTP::Rescuers::RouteNotFound`](https://api.onyxframework.org/Onyx/HTTP/Rescuers/RouteNotFound.html):

```crystal
require "onyx-http"

logger = Onyx::HTTP::Logger.new
request_id = Onyx::HTTP::RequestID.new
response_time = Onyx::HTTP::ResponseTime.new
rescuer = Onyx::HTTP::Rescuers::Standard(Exception).new
router_rescuer = Onyx::HTTP::Rescuers::RouteNotFound.new

router = Onyx::HTTP::Router.new do
  get "/" do |env|
    env.response << "Hello Onyx"
  end

  post "/echo" do |env|
    env.response << env.request.body.try &.gets_to_end
  end

  get "/error" do
    raise "Oops!"
  end
end

server = Onyx::HTTP::Server.new(response_time, request_id, logger, rescuer, router_rescuer, router)

server.bind_tcp(5000)
server.listen
```

```sh
> curl http://localhost:5000/error
500 Internal Server Error
> curl http://localhost:5000/unknown
404 Route Not Found: GET /unknown
```

![Screenshot #5](https://user-images.githubusercontent.com/7955682/52903125-6c63b600-322a-11e9-908a-87d73948c24e.png)

### Macros

Now that you know how Onyx::HTTP works on the low-level, you'd probably want to reduce the amount of boilerplate code in your applications. We've got you covered! [**@onyxframework/onyx**](https://github.com/onyxframework/onyx) shard contains many top-level macros just for your convenience:

```crystal
require "onyx/env"
require "onyx/http"

Onyx.get "/" do |env|
  env.response << "Hello Onyx"
end

Onyx.listen
```

This example includes *all* the handlers listed above!

```sh
> curl http://localhost:5000 -v
* Rebuilt URL to: http://localhost:5000/
*   Trying 127.0.0.1...
* Connected to localhost (127.0.0.1) port 5000 (#0)
> GET / HTTP/1.1
> Host: localhost:5000
> User-Agent: curl/7.47.0
> Accept: */*
>
< HTTP/1.1 200 OK
< Connection: keep-alive
< X-Request-ID: 89bbea1c-26b8-4113-ab67-04279eb0fa13
< Access-Control-Allow-Origin: *
< X-Response-Time: 109
< Content-Length: 12
<
* Connection #0 to host localhost left intact
Hello, Onyx!
```

![Screenshot #6](https://user-images.githubusercontent.com/7955682/52903126-6ff73d00-322a-11e9-8501-907d54a4f342.png)

You can easily modify the server's middleware:

```crystal
Onyx.listen do |handlers, server|
  # This code is going to be injected right before server.listen
  handlers.insert(1, MyCustomHandler.new)
end
```

### Next steps

As said before in the about section, Onyx::HTTP is just a foundation for other Onyx components. You may now want to check [Onyx::REST](https://github.com/onyxframework/rest) for a scalable REST API framework!

The decent Onyx::HTTP API documentation is always available at <https://api.onyxframework.org/http>.

## Community 🍪

There are multiple places to talk about this particular shard and about other ones as well:

* [Onyx::HTTP Gitter chat](https://gitter.im/onyxframework/http)
* [Onyx Framework Gitter community](https://gitter.im/onyxframework)
* [Vlad Faust Gitter community](https://gitter.im/vladfaust)
* [Onyx Framework Twitter](https://twitter.com/onyxframework)
* [Onyx Framework Telegram channel](https://telegram.me/onyxframework)

## Support ❤️

This shard is maintained by me, [Vlad Faust](https://vladfaust.com), a passionate developer with years of programming and product experience. I love creating Open-Source and I want to be able to work full-time on Open-Source projects.

I will do my best to answer your questions in the free communication channels above, but if you want prioritized support, then please consider becoming my patron. Your issues will be labeled with your patronage status, and if you have a sponsor tier, then you and your team be able to communicate with me in private or semi-private channels such as e-mail and [Twist](https://twist.com). There are other perks to consider, so please, don't hesistate to check my Patreon page:

<a href="https://www.patreon.com/vladfaust"><img height="50" src="https://onyxframework.org/img/patreon-button.svg"></a>

You could also help me a lot if you leave a star to this GitHub repository and spread the world about Crystal and Onyx! 📣

## Contributing

1. Fork it ( https://github.com/onyxframework/http/fork )
2. Create your feature branch (git checkout -b my-new-feature)
3. Commit your changes (git commit -am 'feat: some feature') using [Angular style commits](https://github.com/angular/angular/blob/master/CONTRIBUTING.md#commit)
4. Push to the branch (git push origin my-new-feature)
5. Create a new Pull Request

## Contributors

- [Vlad Faust](https://github.com/vladfaust) - creator and maintainer

## Licensing

This software is licensed under [MIT License](LICENSE).

[![Open Source Initiative](https://upload.wikimedia.org/wikipedia/commons/thumb/4/42/Opensource.svg/100px-Opensource.svg.png)](https://opensource.org/licenses/MIT)
