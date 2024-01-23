defmodule HelloWeb.HeadersPlug do
  import Plug.Conn

  def init(opts), do: opts

  def call(conn, _opts) do
    conn
    |> put_resp_header("Server", "Elixir")
  end
end

defmodule HelloWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :hello

  plug Plug.Parsers,
    parsers: [:json, :urlencoded, :multipart],
    pass: ["*/*"],
    json_decoder: Phoenix.json_library()

  plug HelloWeb.HeadersPlug
  plug HelloWeb.Router
end

