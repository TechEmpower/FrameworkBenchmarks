# frozen_string_literal: true

module HelloWorld
  class Routes < Hanami::Routes
    get "/json", to: "json.index"
    get "/db", to: "db.index"
    get "/queries", to: "queries.index"
    get "/fortunes", to: "fortunes.index"
    get "/updates", to: "updates.index"
    get "/plaintext", to: "plaintext.index"
  end
end
