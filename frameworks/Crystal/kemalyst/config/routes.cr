require "../src/controllers/*"

include Kemalyst::Handler

get "/plaintext", benchmark, plaintext
get "/json", benchmark, json
get "/db", benchmark, db
get "/queries", benchmark, queries
get "/updates", benchmark, updates
get "/fortunes", benchmark, fortunes
