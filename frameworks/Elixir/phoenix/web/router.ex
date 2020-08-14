defmodule Hello.Router do
  use Hello.Web, :router

  scope "/", Hello do
    get("/json", PageController, :_json)
    get("/db", PageController, :db)
    get("/queries", PageController, :queries)
    get("/fortunes", PageController, :fortunes)
    get("/updates", PageController, :updates)
    get("/plaintext", PageController, :plaintext)
    get("/", PageController, :index)
  end
end
