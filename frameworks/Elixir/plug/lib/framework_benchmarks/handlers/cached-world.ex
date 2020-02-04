defmodule FrameworkBenchmarks.Handlers.CachedWorld do
  @moduledoc """
  handler for the /cached-worlds route
  """
  def handle(conn) do
    number_of_queries = FrameworkBenchmarks.Handlers.Helpers.parse_queries(conn, "count")

    ids =
      1..number_of_queries
      |> Enum.map(fn _ ->
        :rand.uniform(10_000)
      end)

    {:ok, json} =
      ids
      |> Enum.map(&FrameworkBenchmarks.CachedWorld.get/1)
      |> Eljiffy.encode()

    conn
    |> Plug.Conn.put_resp_content_type("application/json")
    |> Plug.Conn.send_resp(200, json)
  end
end
