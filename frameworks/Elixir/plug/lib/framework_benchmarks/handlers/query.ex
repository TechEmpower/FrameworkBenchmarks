defmodule FrameworkBenchmarks.Handlers.Query do
  @moduledoc """
  handler for the /queries route
  """
  def handle(conn) do
    number_of_queries = FrameworkBenchmarks.Handlers.Helpers.parse_queries(conn, "queries")

    ids =
      1..number_of_queries
      |> Enum.map(fn _ ->
        :rand.uniform(10_000)
      end)

    records =
      ids
      |> Enum.map(fn id ->
        FrameworkBenchmarks.Repo.get(FrameworkBenchmarks.Models.World, id)
      end)

    {:ok, json} =
      records
      |> Enum.map(fn record ->
        record
        |> Map.from_struct()
        |> Map.drop([:__meta__])
      end)
      |> Eljiffy.encode()

    conn
    |> Plug.Conn.put_resp_content_type("application/json")
    |> Plug.Conn.send_resp(200, json)
  end
end
