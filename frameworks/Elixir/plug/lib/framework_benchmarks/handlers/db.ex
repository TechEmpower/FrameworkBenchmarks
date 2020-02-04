defmodule FrameworkBenchmarks.Handlers.DB do
  @moduledoc """
  This is the handler for the /db route
  """
  def handle(conn) do
    id = :rand.uniform(10_000)

    {:ok, json} =
      FrameworkBenchmarks.Repo.get(FrameworkBenchmarks.Models.World, id)
      |> Map.from_struct()
      |> Map.drop([:__meta__])
      |> Eljiffy.encode()

    conn
    |> Plug.Conn.put_resp_content_type("application/json")
    |> Plug.Conn.send_resp(200, json)
  end
end
