# TimeFormat

[![Built with Crystal](https://img.shields.io/badge/built%20with-crystal-000000.svg?style=flat-square)](https://crystal-lang.org/)
[![Build status](https://img.shields.io/travis/vladfaust/time-format.cr/master.svg?style=flat-square)](https://travis-ci.org/vladfaust/time-format.cr)
[![Docs](https://img.shields.io/badge/docs-available-brightgreen.svg?style=flat-square)](https://vladfaust.com/time-format.cr)
[![Releases](https://img.shields.io/github/release/vladfaust/time-format.cr.svg?style=flat-square)](https://github.com/vladfaust/time-format.cr/releases)

Time spans humanizing made simple for [Crystal](https://crystal-lang.org/).

## Installation

Add this to your application's `shard.yml`:

```yaml
dependencies:
  time_format:
    github: vladfaust/time_format.cr
    version: ~> 0.1.0 # Find an actual version in releases
```

This shard follows [Semantic Versioning 2.0.0](https://semver.org/), so see [releases](https://github.com/vladfaust/i18n.cr/releases) and change the `version` accordingly.

## Usage

```crystal
require "time_format"

TimeFormat.auto(0.minutes + 42.seconds) # => 42s
TimeFormat.μs(1.0.milliseconds / 5.0) # => 200μs
```

## Contributing

1. Fork it ( https://github.com/vladfaust/time_format.cr/fork )
2. Create your feature branch (git checkout -b my-new-feature)
3. Commit your changes (git commit -am 'Add some feature')
4. Push to the branch (git push origin my-new-feature)
5. Create a new Pull Request

## Contributors

- [@vladfaust](https://github.com/vladfaust) Vlad Faust - creator, maintainer
