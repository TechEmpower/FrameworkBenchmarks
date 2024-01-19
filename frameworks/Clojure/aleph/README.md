# Aleph Benchmarking Test

This is the [Aleph](https://github.com/clj-commons/aleph) portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

## Infrastructure Software Versions
The dependencies are documented in [project.clj](project.clj),
but the main ones are:

* [Aleph 0.6.1](https://github.com/clj-commons/aleph)
* [Clojure 1.11.1](http://clojure.org/)
* [metosin/jsonista 0.3.7](https://github.com/metosin/jsonista), which in turn uses [Jackson](http://jackson.codehaus.org/)
* [hiccup 1.0.5](https://github.com/weavejester/hiccup)
* [porsas 0.0.1-alpha14](https://github.com/arnaudgeiser/porsas)

## Test URLs
### JSON Encoding Test
`http://localhost:8080/json`

### Single Query Test
`http://localhost:8080/db`

### Multiple Query Test
`http://localhost:8080/queries?queries=number`

### Fortune Test
`http://localhost:8080/fortunes`

### Database Updates
`http://localhost:8080/updates?queries=number`

### Plaintext
`http://localhost:8080/plaintext`
