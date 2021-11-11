defmodule FrameworkBenchmarks.Handlers.PlainText do
  @moduledoc """
  This is the handle for the /plaintext route
  """
  def handle(conn) do
    conn
    |> Plug.Conn.put_resp_content_type("text/plain")
    |> Plug.Conn.send_resp(200, "Hello, World!")
  end
end
