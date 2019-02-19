<a href="https://onyxframework.org"><img width="100" height="100" src="https://onyxframework.org/img/logo.svg"></a>

# Onyx

[![Built with Crystal](https://img.shields.io/badge/built%20with-crystal-000000.svg?style=flat-square)](https://crystal-lang.org/)
[![Travis CI build](https://img.shields.io/travis/onyxframework/onyx/master.svg?style=flat-square)](https://travis-ci.org/onyxframework/onyx)
[![API docs](https://img.shields.io/badge/api_docs-online-brightgreen.svg?style=flat-square)](https://api.onyxframework.org/onyx)
[![Latest release](https://img.shields.io/github/release/onyxframework/onyx.svg?style=flat-square)](https://github.com/onyxframework/onyx/releases)

Macros for easier development.

## Supporters ❤️

Thanks to all my patrons, I can continue working on beautiful Open Source Software! 🙏

[Alexander Maslov](https://seendex.ru), [Lauri Jutila](https://github.com/ljuti)

*You can become a patron too in exchange of prioritized support and other perks*

<a href="https://www.patreon.com/vladfaust"><img height="50" src="https://onyxframework.org/img/patreon-button.svg"></a>

## About 👋

This shard (should be called *Onyx/Onyx*) includes a number of convenient macros for easier day-to-day development with the [Onyx Framework](https://onyxframework.org). It convers almost all of its components:

* Top-level macros:
  * [Env](#env) to load environment variables from `.env` files
  * [Logger](#logger) to enable `Onyx.logger`
  * [DB](#db) to enable `Onyx.db`
* [Onyx::HTTP macros](#http) for use with [Onyx::HTTP](https://github.com/onyxframework/http)
* [Onyx::REST macros](#rest) for use with [Onyx::REST](https://github.com/onyxframework/rest)
* [Onyx::SQL macros](#sql) for use with [Onyx::SQL](https://github.com/onyxframework/sql)

## Installation 📥

Add this to your application's `shard.yml`:

```yaml
dependencies:
  onyx:
    github: onyxframework/onyx
    version: ~> 0.1.0
```

This shard follows [Semantic Versioning v2.0.0](http://semver.org/), so check [releases](https://github.com/onyxframework/rest/releases) and change the `version` accordingly. Please visit [github.com/crystal-lang/shards](https://github.com/crystal-lang/shards) to know more about Crystal shards.

Note that this shard does **not** have implicit dependencies for other framework components. For example, to use `"onyx/http"` macros, you must add `onyx-http` dependendency as well:

```yaml
dependencies:
  onyx:
    github: onyxframework/onyx
    version: ~> 0.1.0
  onyx-http:
    github: onyxframework/http
    version: ~> 0.1.0
```

## Usage 💻

### Env

Firstly sets `CRYSTAL_ENV` environment variable to `"development"` if not set yet. Then loads other environment variables from `.env` files in this order, overwriting if defined multiple times:

1. `.env` file
2. `.env.local` file
3. `.env.#{CRYSTAL_ENV}` file
3. `.env.#{CRYSTAL_ENV}.local` file

It also enables `.runtime_env` and `.buildtime_env` top-level macros which raise if an environment variable is not defined.

```crystal
require "onyx/env"

# At this point, all variables are loaded from .env files
#

runtime_env DATABASE_URL # Will raise on program start if DATABASE_URL variable is missing
```

This feature is powered by [dotenv](https://github.com/gdotdesign/cr-dotenv) shard by @gdotdesign.

### Logger

Enables using the singleton `Onyx.logger` instance. It automatically requires [Env](#env).

```crystal
require "onyx/logger"

Onyx.logger.info("Hello world!")
```

```sh
DEBUG [12:45:52.520 #13543] Hello world!
```

### DB

Enables using the singleton `Onyx.db` instance. It automatically requires [Env](#env) and **raises** if no `DATABASE_URL` environment variable is defined *or* the database is not reachable.

```crystal
require "onyx/db"

Onyx.db.query("SELECT 1")
```

### HTTP

Enables the singleton [Onyx::HTTP](https://github.com/onyxframework/http) instance. It automatically requires [Env](#env) and [Logger](#logger) and adds the following methods:

* `Onyx.get`, `Onyx.post`, `Onyx.put`, `Onyx.patch`, `Onyx.delete` and `Onyx.options` which call the according [`Onyx::HTTP::Router`](https://api.onyxframework.org/http/Onyx/HTTP/Router.html) method
* `Onyx.draw` which calls `Onyx::HTTP::Router#draw`
* `Onyx.listen` which launches the [`Onyx::HTTP::Server`](https://api.onyxframework.org/http/Onyx/HTTP/Server.html) instance

Example:

```crystal
require "onyx/http"

Onyx.get "/" do |env|
  env.response << "Hello Onyx"
end

Onyx.listen
```

```sh
> curl http://localhost:5000
Hello Onyx
```

You **must** manually add [Onyx::HTTP](https://github.com/onyxframework/http) as a dependency in your `shard.yml`.

### REST

Enables the singleton [Onyx::HTTP](https://github.com/onyxframework/rest) instance. It automatically requires [HTTP](#http) and adds `Onyx.render` method:

```crystal
require "onyx/rest"

struct MyView
  include Onyx::REST::View

  def initialize(@foo : String)
  end

  text("foo = #{@foo}")
end

Onyx.render(:text) # Or :json

Onyx.get "/" do |env|
  env.response.view = MyView.new("bar")
end

Onyx.listen
```

You **must** manually add [Onyx::REST](https://github.com/onyxframework/rest) as a dependency in your `shard.yml`.

### SQL

Enables the singleton [Onyx::SQL](https://github.com/onyxframework/sql) [`Repository`](https://api.onyxframework.org/sql/Onyx/SQL/Repository.html) instance accessible with `Onyx.repo` method and also adds proxy `Onyx.query`, `Onyx.exec` and `Onyx.repo` methods. It automatically requires [Env](#env) and [DB](#db) (meaning that you **must** have `DATABASE_URL` variable defined).

```crystal
require "onyx/sql"

class User
  include Onyx::SQL::Model

  schema users do
    pkey id : Int32
    type name : String
  end
end

users = Onyx.query(User.where(id: 42)).first?
```

## Community 🍪

There are multiple places to talk about this particular shard and about other ones as well:

* [Main Onyx Gitter chat](https://gitter.im/onyxframework/Lobby)
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
