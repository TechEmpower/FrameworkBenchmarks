# frozen_string_literal: true

module HelloWorld
  class Routes < Hanami::Routes
    get "/db", to: "db.index"
    get "/json", to: "json.index"
    get "/updates", to: "updates.index"
    get "/plaintext", to: "plaintext.index"
    get "/queries", to: "queries.index"
  end
end
