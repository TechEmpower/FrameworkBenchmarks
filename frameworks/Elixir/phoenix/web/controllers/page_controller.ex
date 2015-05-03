defmodule Hello.PageController do
  use Phoenix.Controller

  plug :action

  def index(conn, _params) do
    json conn, %{
      "TE Benchmarks\n" => "Started",
      "DBHOST" => System.get_env("DBHOST"),
      "DBPORT" => System.get_env("DBPORT"),
    }
  end

  # avoid namespace collision
  def _json(conn, _params) do
    json conn, %{message: "Hello, world!"}
  end

  def db(conn, _params) do
    :random.seed(:erlang.now)
    id = :random.uniform(10000)
    text conn, "TE Benchmarks\n"
  end

  def queries(conn, _params) do
    text conn, "TE Benchmarks\n"
  end

  def fortunes(conn, _params) do
    text conn, "TE Benchmarks\n"
  end

  def updates(conn, _params) do
    text conn, "TE Benchmarks\n"
  end

  def plaintext(conn, _params) do
    text conn, "Hello, world!"
  end
end
