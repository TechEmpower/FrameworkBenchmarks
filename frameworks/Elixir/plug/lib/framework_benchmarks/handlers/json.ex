defmodule FrameworkBenchmarks.Handlers.JSON do
  @moduledoc """
  This is the handle for the /json route
  """
  def handle(conn) do
    json = Jason.encode_to_iodata!(%{message: "Hello, World!"})

    conn
    |> Plug.Conn.put_resp_content_type("application/json")
    |> Plug.Conn.send_resp(200, json)
  end
end
