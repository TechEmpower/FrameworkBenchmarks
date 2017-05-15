require "../src/controllers/*"

include Kemalyst::Handler

get "/plaintext", test, plaintext
get "/json", test, json
get "/db", test, db
get "/queries", test, queries
get "/updates", test, updates
get "/fortunes", test, fortunes
