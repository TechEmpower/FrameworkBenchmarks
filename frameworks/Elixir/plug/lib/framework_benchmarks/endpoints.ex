defmodule FrameworkBenchmarks.Endpoints do
  @moduledoc """
  The plug endpoints for the benchmarks
  """
  use Plug.Router

  alias FrameworkBenchmarks.Handlers.{JSON, DB, Query, CachedWorld, PlainText, Fortune, Update}

  plug(:match)
  plug(:dispatch)

  get "/json" do
    JSON.handle(conn)
  end

  get "/db" do
    DB.handle(conn)
  end

  get "/queries" do
    Query.handle(conn)
  end

  get "/cached-worlds" do
    CachedWorld.handle(conn)
  end

  get "/plaintext" do
    PlainText.handle(conn)
  end

  get "/fortunes" do
    Fortune.handle(conn)
  end

  get "/updates" do
    Update.handle(conn)
  end

  match _ do
    send_resp(conn, 404, "<h1>Page Not Found</h1>")
  end
end
