defmodule Hello.Router do
  use Plug.Router

  plug(:match)
  plug(:dispatch)

  get "/" do
    conn
    |> put_resp_content_type("text/html")
    |> send_resp(200, "<h2>/</h2>")
  end

  get "/json" do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Poison.encode!(%{message: "Hello, World!"}))
  end

  get "/plaintext" do
    conn
    |> put_resp_content_type("text/plain")
    |> send_resp(200, "Hello, World!")
  end

  match(_, do: send_resp(conn, 404, "Oops!"))
end
