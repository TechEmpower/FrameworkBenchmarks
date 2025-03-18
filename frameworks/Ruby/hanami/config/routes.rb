# frozen_string_literal: true

module HelloWorld
  class Routes < Hanami::Routes
    get "/json", to: ->(env) do
      [200,
       {
         'Server' => 'Rails',
         'Content-Type' => 'application/json',
         'Date' => Time.now.httpdate,
       },
       [{ 'message' => 'Hello, World!' }.to_json]]
    end
    get "/db", to: "db.index"
    get "/queries", to: "queries.index"
    get "/fortunes", to: "fortunes.index"
    get "/updates", to: "updates.index"
    get "/plaintext", to: ->(env) do
      [200,
       {
         'Server' => 'Hanami',
         'Content-Type' => 'text/plain',
         'Date' => Time.now.httpdate
       },
       ['Hello, World!']]
    end
  end
end
