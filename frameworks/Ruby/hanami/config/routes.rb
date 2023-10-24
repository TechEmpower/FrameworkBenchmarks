# frozen_string_literal: true

module HelloWorld
  class Routes < Hanami::Routes
    get "/db", to: "db.index"
    get "/json", to: "json.index"
    get "/plaintext", to: "plaintext.index"
  end
end
