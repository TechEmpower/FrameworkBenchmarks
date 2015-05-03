defmodule Hello.Router do
  use Phoenix.Router

  pipeline :browser do
    plug :accepts, ~w(html)
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
  end

  pipeline :api do
    plug :accepts, ~w(json)
  end

  scope "/", Hello do
    pipe_through :browser # Use the default browser stack

    get "/json", PageController, :_json
    get "/db", PageController, :db
    get "/queries", PageController, :queries
    get "/fortunes", PageController, :fortunes
    get "/updates", PageController, :updates
    get "/plaintext", PageController, :plaintext
    get "/", PageController, :index
  end

  # Other scopes may use custom stacks.
  # scope "/api", Hello do
  #   pipe_through :api
  # end
end
