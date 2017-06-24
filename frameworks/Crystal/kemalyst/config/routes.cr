require "../src/controllers/*"

include Kemalyst::Handler

get "/plaintext", TestController::Plaintext.new
get "/json", TestController::Json.new
get "/db", TestController::Db.new
get "/queries", TestController::Queries.new
get "/updates", TestController::Updates.new
get "/fortunes", TestController::Fortunes.new

# get "/plaintext", "benchmark", "plaintext"
# get "/json", "benchmark", "json"
# get "/db", "benchmark", "db"
# get "/queries", "benchmark", "queries"
# get "/updates", "benchmark", "updates"
# get "/fortunes", "benchmark", "fortunes"
