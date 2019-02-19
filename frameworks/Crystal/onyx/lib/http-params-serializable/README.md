# HTTP::Params::Serializable

[![Built with Crystal](https://img.shields.io/badge/built%20with-crystal-000000.svg?style=flat-square)](https://crystal-lang.org/)
[![Build status](https://img.shields.io/travis/com/vladfaust/http-params-serializable/master.svg?style=flat-square)](https://travis-ci.com/vladfaust/http-params-serializable)
[![Docs](https://img.shields.io/badge/docs-available-brightgreen.svg?style=flat-square)](https://github.vladfaust.com/http-params-serializable)
[![Releases](https://img.shields.io/github/release/vladfaust/http-params-serializable.svg?style=flat-square)](https://github.com/vladfaust/http-params-serializable/releases)
[![Awesome](https://awesome.re/badge-flat2.svg)](https://github.com/veelenga/awesome-crystal)
[![vladfaust.com](https://img.shields.io/badge/style-.com-lightgrey.svg?longCache=true&style=flat-square&label=vladfaust&colorB=0a83d8)](https://vladfaust.com)
[![Patrons count](https://img.shields.io/badge/dynamic/json.svg?label=patrons&url=https://www.patreon.com/api/user/11296360&query=$.included[0].attributes.patron_count&style=flat-square&colorB=red&maxAge=86400)](https://www.patreon.com/vladfaust)
[![Gitter chat](https://img.shields.io/badge/chat%20on-gitter-green.svg?colorB=ED1965&logo=gitter&style=flat-square)](https://gitter.im/vladfaust/http-params-serializable)

The HTTP params serialization module for [Crystal](https://crystal-lang.org/).

## Supporters

Thanks to all my patrons, I can build and support beautiful Open Source Software! 🙏

[Lauri Jutila](https://github.com/ljuti)

*You can become a patron too in exchange of prioritized support and other perks*

[![Become Patron](https://vladfaust.com/img/patreon-small.svg)](https://www.patreon.com/vladfaust)

## About

This module is intended to provide a simple and convenient way to make an object to safely initialize from an HTTP params query (be it an URL query or `"application/x-www-form-urlencoded"` body). It tries to have an API almost the same as existing [`JSON::Serializable`](https://crystal-lang.org/api/0.27.0/JSON/Serializable.html) and [`YAML::Serializable`](https://crystal-lang.org/api/0.27.0/YAML/Serializable.html) modules, thus allowing to serialize infinitely-nested structures, including Arrays and Hashes.

## Installation

1. Add the dependency to your `shard.yml`:

```yaml
dependencies:
  http-params-serializable:
    github: vladfaust/http-params-serializable
    version: ~> 0.3.0
```

2. Run `shards install`

## Usage

Simple example:

```crystal
require "http-params-serializable"

struct Params
  include HTTP::Params::Serializable
  getter id : Int32
end

params = Params.from_query("id=42")
pp params.id.class # => Int32
pp params.to_query # => "id=42"

Params.from_query("")
# HTTP::Params::Serializable::ParamMissingError: Parameter "id" is missing

Params.from_query("id=foo")
# HTTP::Params::Serializable::ParamTypeCastError: Parameter "id" cannot be cast from "foo" to Int32
```

As you may expect, unions work as well:

```crystal
struct Params
  include HTTP::Params::Serializable
  getter id : Int32 | Nil
end

params = Params.from_query("id=")
pp params.id # => nil
```

Arrays are supported too:

```crystal
struct Params
  include HTTP::Params::Serializable
  getter foo : Array(Float32)
end

params = Params.from_query("foo[]=42.0&foo[]=43.5")
pp params.foo[1] # => 43.5
pp params.to_query # => "foo[]=42.0&foo[]=43.5"
```

Nested params are supported:

```crystal
struct Params
  include HTTP::Params::Serializable
  getter nested : Nested

  struct Nested
    include HTTP::Params::Serializable
    getter foo : Bool
  end
end

params = Params.from_query("nested[foo]=true")
pp params.nested.foo # => true
pp params.to_query # => "nested[foo]=true"
```

Nested arrays are supported as well:

```crystal
struct Params
  include HTTP::Params::Serializable
  getter nested : Array(Nested)

  struct Nested
    include HTTP::Params::Serializable
    getter foo : Array(Int32)
  end
end

params = Params.from_query("nested[0][foo][]=1&nested[0][foo][]=2")
pp params.nested.first.foo.first # => [1, 2]
pp params.to_query # ditto
```

It is also possible to alter the serialization behaviour with [`@[HTTP::Param]`](http://github.vladfaust.com/http-params-serializable/HTTP/Param.html) annotation. It currently supports two options: `:key` and `:converter`. Read more in [docs](http://github.vladfaust.com/http-params-serializable/HTTP/Param.html).

```crystal
struct Params
  include HTTP::Params::Serializable

  @[HTTP::Param(key: "the___Time", converter: Time::EpochConverter)]
  getter time : Time
end

params = Params.from_query("the___Time=1544958806")
pp params.time # => 2018-12-16 11:13:26.0 UTC
pp params.to_query # => "the___Time=1544958806"
```

### Custom serialization rules

If you want to know-how to make custom objects serializable, read [the Custom serialization rules Wiki page](https://github.com/vladfaust/http-params-serializable/wiki/Custom-serialization-rules).

### Usage with [Onyx::REST](https://onyxframework.org)

[Onyx::REST](https://github.com/onyxframework/rest) comes with an Action module, which implements common param sources parsing, which uses this shard under the hood:

```crystal
require "onyx/rest"

struct UpdateUser
  include Onyx::REST::Action

  params do
    # Path params ("/users/:id")
    path do
      type id : Int32
    end

    # Query params ("/users/42?password=secret")
    # Nesting and arrays natively supported
    query do
      type password : String
      type foo do
        type bar : Int32?
      end
    end

    # Full support for form-data payloads
    form do
      type username : String?
      type email : String?
    end
  end

  errors do
    type Forbidden(403)
  end

  def call
    user = Onyx.query(User.where(id: params.path.id))
    raise Forbidden.new unless user.password == params.query.password
  end
end

Onyx.put "/users/:id", UpdateUser
Onyx.listen
```

### Usage with [Kemal](http://kemalcr.com)

It's pretty simple to make your Kemal applications more safe:

```crystal
require "kemal"
require "http-params-serializable"

struct Params
  include HTTP::Params::Serializable
  getter id : Int32
end

get "/" do |env|
  if query = env.request.query
    query_params = Params.from_query(query)

    if query_params.id > 0
      "#{query_params.id} is positive\n"
    else
      "#{query_params.id} is negative or zero\n"
    end
  else
    "Empty query\n"
  end
rescue ex : HTTP::Params::Serializable::Error
  ex.message.not_nil! + "\n"
end

Kemal.run
```

```console
$ curl http://localhost:3000?id=42
42 is positive
$ curl http://localhost:3000?id=-1
-1 is negative or zero
$ curl http://localhost:3000?id=foo
Parameter "id" cannot be cast from "foo" to Int32
```

## Development

`crystal spec` and you're good to go.

## Contributing

1. Fork it (<https://github.com/vladfaust/http-params-serializable/fork>)
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create a new Pull Request

## Contributors

- [Vlad Faust](https://github.com/vladfaust) - creator and maintainer
