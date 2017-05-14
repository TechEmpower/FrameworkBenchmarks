require "../src/controllers/*"

include Kemalyst::Handler

get "/plaintext", test, headers
get "/json", test, headers
get "/db", test, headers
get "/queries", test, headers
get "/updates", test, headers
get "/fortunes", test, headers

get "/plaintext", test, plaintext
get "/json", test, json
get "/db", test, db
get "/queries", test, queries
get "/updates", test, updates
get "/fortunes", test, fortunes
