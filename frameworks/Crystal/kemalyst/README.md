# kemalyst

Kemalyst is a Rails like framework for Crystal.  It provides many of the same capabilities as rails, written in a language similar to ruby but is compiled to native binary.  It gives you C speed but Ruby ease.

## Installation

Create a pg database called `demo` and configure the `config/database.yml`
to provide the credentials to access the table.

Then:
```
shards update
kgen migrate up
```

## Usage

To run the demo:
```
crystal build src/kemalyst.cr
./kemalyst
```

## Docker and Docker Compose

This will start an instance of postgres, migrate the database, run the specs,
and launch the site at http://localhost:3000
```
docker-compose up -d
```

To view the logs:
```
docker-compose logs -f
```

Note: The Docker images are compatible with Heroku.  

## Contributing

1. Fork it ( https://github.com/drujensen/FrameworkBenchmarks/fork )
2. Create your feature branch (git checkout -b my-new-feature)
3. Commit your changes (git commit -am 'Add some feature')
4. Push to the branch (git push origin my-new-feature)
5. Create a new Pull Request

## Contributors

- [drujensen](https://github.com/drujensen) Dru Jensen - creator, maintainer
