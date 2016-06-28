defmodule Hello.Router do
  use Hello.Web, :router

  scope "/", Hello do
    get "/json", PageController, :_json
    get "/db", PageController, :db
    get "/queries", PageController, :queries
    get "/fortune", PageController, :fortunes
    get "/update", PageController, :updates
    get "/plaintext", PageController, :plaintext
    get "/", PageController, :index
  end
end
