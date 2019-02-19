<a href="https://onyxframework.org"><img width="100" height="100" src="https://onyxframework.org/img/logo.svg"></a>

# Onyx::REST

[![Built with Crystal](https://img.shields.io/badge/built%20with-crystal-000000.svg?style=flat-square)](https://crystal-lang.org/)
[![Travis CI build](https://img.shields.io/travis/onyxframework/rest/master.svg?style=flat-square)](https://travis-ci.org/onyxframework/rest)
[![API docs](https://img.shields.io/badge/api_docs-online-brightgreen.svg?style=flat-square)](https://api.onyxframework.org/rest)
[![Latest release](https://img.shields.io/github/release/onyxframework/rest.svg?style=flat-square)](https://github.com/onyxframework/rest/releases)

A REST API framework for [Crystal](https://crystal-lang.org).

## Supporters ❤️

Thanks to all my patrons, I can continue working on beautiful Open Source Software! 🙏

[Alexander Maslov](https://seendex.ru), [Lauri Jutila](https://github.com/ljuti)

*You can become a patron too in exchange of prioritized support and other perks*

<a href="https://www.patreon.com/vladfaust"><img height="50" src="https://onyxframework.org/img/patreon-button.svg"></a>

## About 👋

Onyx::REST is an opinionated REST API framework build on top of [Onyx::HTTP](https://github.com/onyxframework/http). It provides modules to build scalabale applications, such as [`Action`](https://api.onyxframework.org/rest/Onyx/REST/Action.html) and [`View`](https://api.onyxframework.org/rest/Onyx/REST/View.html).

## Installation 📥

Add this to your application's `shard.yml`:

```yaml
dependencies:
  onyx-rest:
    github: onyxframework/rest
    version: ~> 0.6.0
```

This shard follows [Semantic Versioning v2.0.0](http://semver.org/), so check [releases](https://github.com/onyxframework/rest/releases) and change the `version` accordingly. Please visit [github.com/crystal-lang/shards](https://github.com/crystal-lang/shards) to know more about Crystal shards.

## Usage 💻

> ⚠ **Note:** you should make yourself familiar with [Onyx::HTTP](https://github.com/onyxframework/http) before using this shard.

Onyx::REST has two main concepts: actions and views. Action is where the business logic takes place, they are essentially REST endpoints. And views take care of rendering.

### Actions

[`Action`](https://api.onyxframework.org/rest/Onyx/REST/Action.html) is a module and can be included into any object. Structs works nice, because actions do not need to reference each other, they are throw-away objects. There is no enforced one-to-one relation between actions and endpoints. Multiple endpoints can execute a single Action, and vice-versa. Let's define a simple action:

```crystal
require "onyx-rest"

struct Hello
  include Onyx::REST::Action

  def call
    context.response << "Hello Onyx"
  end
end

router = Onyx::HTTP::Router.new do
  get "/", Hello
end

server = Onyx::HTTP::Server.new(router)
server.bind_tcp(5000)
server.listen
```

```sh
> curl http://localhost:5000
Hello Onyx
```

Actions have access to the current [`HTTP::Server::Context`](https://crystal-lang.org/api/0.27.2/HTTP/Server/Context.html) via `#context` getter. They also have have `#status`, `#header` and `#redirect` shortcuts.

#### Params

You can easily define strongly-typed parameters in Actions to enable it parsing incoming values from the HTTP request path, query and also form and JSON bodies. Nested and array params are supported as well.

```crystal
require "onyx-rest"

struct Hello
  include Onyx::REST::Action

  params do
    query do
      type who : String
    end
  end

  def call
    context.response << "Hello #{params.query.who}"
  end
end

router = Onyx::HTTP::Router.new do
  get "/", Hello
end

server = Onyx::HTTP::Server.new(router)
server.bind_tcp(5000)
server.listen
```

```sh
> curl http://localhost:5000/?who=World
Hello World
```

This feature is proudly powered by [HTTP::Params::Serializable](https://github.com/vladfaust/http-params-serializable) shard. Read more about `.params` macro in [API docs](https://api.onyxframework.org/rest/Onyx/REST/Action.html).

#### Errors

It's typical for web applications to have *expected errors*, e.g. "User is not found". Actions have syntax to define such errors (you'll need an [`Onyx::REST::Rescuer`](https://api.onyxframework.org/rest/Onyx/REST/Rescuer.html)):

```crystal
require "onyx-rest"

struct GetUser
  include Onyx::REST::Action

  params do
    path do
      type id : Int32
    end
  end

  errors do
    type UserNotFound(404)
  end

  def call
    if params.path.id == 42
      context.response << "Found user"
    else
      raise UserNotFound.new
    end
  end
end

rescuer = Onyx::REST::Rescuer.new

router = Onyx::HTTP::Router.new do
  get "/users/:id", GetUser
end

server = Onyx::HTTP::Server.new(rescuer, router)
server.bind_tcp(5000)
server.listen
```

```sh
> curl http://localhost:5000/users/42
Found user
> curl http://localhost:5000/users/43
404 User Not Found
```

### Views

[`View`](https://api.onyxframework.org/rest/Onyx/REST/View.html) is a module which responsibility is to render responses. A view should be handled by a *renderer*, which could either be [`Onyx::Renderers::Text`](https://api.onyxframework.org/rest/Onyx/REST/Renderers/Text.html) or [`Onyx::Renderers::JSON`](https://api.onyxframework.org/rest/Onyx/REST/Renderers/JSON.html):

```crystal
require "onyx-rest"
require "onyx-rest/renderers/text"

struct UserView
  include Onyx::REST::View

  def initialize(@id : Int32, @name : String)
  end

  text("id: #{@id}, name: #{@name}")
end

router = Onyx::HTTP::Router.new do
  get "/" do |env|
    env.response.view = UserView.new(42, "John")
  end
end

renderer = Onyx::REST::Renderers::Text.new

server = Onyx::HTTP::Server.new(router, renderer)
server.bind_tcp(5000)
server.listen
```

```sh
> curl http://localhost:5000
id: 42, name: John
```

### Actions + Views

Actions have two native integrations with views. The first one is `#view` method which sets the response view like this:

```crystal
  def call
    view(UserView.new(42, "John"))
  end
```

And the second one is the fact that if an action returns a `View`, it is then treated as a response view (if not set eariler):

```crystal
  def call
    return UserView.new(42, "John")
  end
```

or, for a prettier control flow:

```crystal
  def call
    return UserView.new(42, "John") if something
    some_code
    return AnotherView.new
  end
```

### Macros

You should use `"onyx/rest"` instead of `"onyx/http"` to enable [`Onyx::REST::Rescuer`](https://api.onyxframework.org/rest/Onyx/REST/Rescuer.html) and an ability to enable a renderer in one line:

```crystal
require "onyx/rest"

Onyx.render(:json)
Onyx.listen
```

Read more about Onyx::HTTP macros at [**@onyxframework/onyx#http**](https://github.com/onyxframework/onyx#http) or at [**@onyxframework/http#macros**](https://github.com/onyxframework/http#macros).

## Community 🍪

There are multiple places to talk about this particular shard and about other ones as well:

* [Onyx::HTTP Gitter chat](https://gitter.im/onyxframework/rest)
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
