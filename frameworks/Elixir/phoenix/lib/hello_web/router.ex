defmodule HelloWeb.Router do
  use HelloWeb, :router

  scope "/", HelloWeb do
    get("/json", PageController, :_json)
    get("/db", PageController, :db)
    get("/queries", PageController, :queries)
    get("/fortunes", PageController, :fortunes)
    get("/updates", PageController, :updates)
    get("/plaintext", PageController, :plaintext)
    get("/cached-queries", PageController, :cached)
    get("/", PageController, :index)
  end
end
