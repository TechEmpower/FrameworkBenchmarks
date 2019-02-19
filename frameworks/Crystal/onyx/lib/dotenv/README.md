# Dotenv

[![Build Status](https://travis-ci.org/gdotdesign/cr-dotenv.svg?branch=master)](https://travis-ci.org/gdotdesign/cr-dotenv)

Loads `.env` file.

## Installation


Add this to your application's `shard.yml`:

```yaml
dependencies:
  dotenv:
    github: gdotdesign/cr-dotenv
```


## Usage

Your `.env` file:
```
# Comments can be included for context
#
MY_VARIABLE=my-value

# Empty Lines are also ignore
#
ANOTHER_VAR=awesome-value
```

In your application:
```crystal
require "dotenv"

# Load deafult ".env" file
Dotenv.load

# Other file
Dotenv.load ".env-other"

# If you load env variable from file and
# you want to raise execption in case of
# missing dotenv file, to make this error
# immediately obvious then use the bang 
# verion of the load method:
Dotenv.load!
# or
Dotenv.load! ".env-other"

# From IO
Dotenv.load MemoryIO.new("VAR=test")

# From Hash
Dotenv.load({"VAR" => "test"})

# A Hash is returned with the loaded variables
hash = Dotenv.load

puts hash["MY_VARIABLE"] # my-value
puts ENV["MY_VARIABLE"] # my-value
```

## Contributing

1. Fork it ( https://github.com/gdotdesign/cr-dotenv/fork )
2. Create your feature branch (git checkout -b my-new-feature)
3. Commit your changes (git commit -am 'Add some feature')
4. Push to the branch (git push origin my-new-feature)
5. Create a new Pull Request

## Contributors

- [[gdotdesign]](https://github.com/[gdotdesign]) Gusztáv Szikszai - creator, maintainer
- [[bonyiii]](https://github.com/[bonyiii]) 
- [[kriskova]](https://github.com/kriskova)
- [[neovintage]](https://github.com/[neovintage]) Rimas Silkaitis
