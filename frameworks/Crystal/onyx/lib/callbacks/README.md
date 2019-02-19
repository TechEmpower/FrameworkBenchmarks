# Callbacks

[![Built with Crystal](https://img.shields.io/badge/built%20with-crystal-000000.svg?style=flat-square)](https://crystal-lang.org/)
[![Build status](https://img.shields.io/travis/vladfaust/callbacks.cr/master.svg?style=flat-square)](https://travis-ci.org/vladfaust/callbacks.cr)
[![Docs](https://img.shields.io/badge/docs-available-brightgreen.svg?style=flat-square)](https://github.vladfaust.com/callbacks.cr)
[![Releases](https://img.shields.io/github/release/vladfaust/callbacks.cr.svg?style=flat-square)](https://github.com/vladfaust/callbacks.cr/releases)
[![Awesome](https://awesome.re/badge-flat2.svg)](https://github.com/veelenga/awesome-crystal)
[![vladfaust.com](https://img.shields.io/badge/style-.com-lightgrey.svg?longCache=true&style=flat-square&label=vladfaust&colorB=0a83d8)](https://vladfaust.com)
[![Patrons count](https://img.shields.io/badge/dynamic/json.svg?label=patrons&url=https://www.patreon.com/api/user/11296360&query=$.included[0].attributes.patron_count&style=flat-square&colorB=red&maxAge=86400)](https://www.patreon.com/vladfaust)
[![Gitter chat](https://img.shields.io/badge/chat%20on-gitter-green.svg?colorB=ED1965&logo=gitter&style=flat-square)](https://gitter.im/vladfaust/Lobby)

An expressive callbacks module for [Crystal](https://crystal-lang.org/).

## Supporters

Thanks to all my patrons, I can build and support beautiful Open Source Software! 🙏

[Lauri Jutila](https://github.com/ljuti)

*You can become a patron too in exchange of prioritized support and other perks*

[![Become Patron](https://vladfaust.com/img/patreon-small.svg)](https://www.patreon.com/vladfaust)

## About

Callbacks defined with this module are properly inherited and run within a scope of the object itself (i.e. have an access to instance variables etc.).

## Installation

Add this to your application's `shard.yml`:

```yaml
dependencies:
  callbacks:
    github: vladfaust/callbacks.cr
    version: ~> 0.2.0
```

This shard follows [Semantic Versioning 2.0.0](https://semver.org/), so see [releases](https://github.com/vladfaust/callbacks.cr/releases) and change the `version` accordingly.

## Usage

```crystal
require "callbacks"

class Foo
  include Callbacks

  def call
    with_callbacks { puts "call" }
  end

  before do
    puts "1"
  end

  before do
    puts "2"
  end

  after do
    puts "3"
  end

  after do
    puts "4"
  end
end

Foo.new.call
# 1, 2, call, 3, 4
```

Objects including `Callbacks` module can also be inherited preserving all callbacks:

```crystal
class Bar < Foo
  # Childrens before callbacks have higher precedence
  before do
    puts "5"
  end

  # Childrens after callbacks executed after parents'
  after do
    puts "6"
  end
end

Bar.new.call
# 5, 1, 2, call, 3, 4, 6
```

## Contributing

1. Fork it (<https://github.com/vladfaust/callbacks.cr/fork>)
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create a new Pull Request

## Contributors

- [@vladfaust](https://github.com/vladfaust) Vlad Faust - creator, maintainer
