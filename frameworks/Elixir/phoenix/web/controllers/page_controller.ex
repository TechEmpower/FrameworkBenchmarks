defmodule Hello.PageController do
  use Hello.Web, :controller
  alias Hello.World
  alias Hello.Fortune

  def index(conn, _params) do
    conn
    |> merge_resp_headers(%{"content-type" => "application/json"})
    |> json(%{"TE Benchmarks\n" => "Started"})
  end

  # avoid namespace collision
  def _json(conn, _params) do
    conn
    |> merge_resp_headers(%{"content-type" => "application/json"})
    |> json(%{message: "Hello, world!"})
  end

  def db(conn, _params) do
    conn
    |> merge_resp_headers(%{"content-type" => "application/json"})
    |> json(Repo.get(World, :rand.uniform(10000)))
  end

  def queries(conn, params) do
    q = try do
      case String.to_integer(params["queries"]) do
        x when x < 1    -> 1
        x when x > 500  -> 500
        x               -> x
      end
    rescue
      ArgumentError -> 1
    end

    conn
    |> merge_resp_headers(%{"content-type" => "application/json"})
    |> json(Enum.map(1..q, fn _ -> Repo.get(World, :rand.uniform(10000)) end))
  end

  def fortunes(conn, _params) do
    additional_fortune = %Fortune{
      id: 0,
      message: "Additional fortune added at request time."
    }

    fortunes = [additional_fortune | Repo.all(Fortune)]

    render conn, "fortunes.html", fortunes: Enum.sort(fortunes, fn f1, f2 -> f1.message < f2.message end)
  end

  def updates(conn, params) do
    q = try do
      case String.to_integer(params["queries"]) do
        x when x < 1    -> 1
        x when x > 500  -> 500
        x               -> x
      end
    rescue
      ArgumentError -> 1
    end

    conn
    |> merge_resp_headers(%{"content-type" => "application/json"})
    |> json(Enum.map(1..q, fn _ ->
      w = Repo.get(World, :rand.uniform(10000))
      changeset = World.changeset(w, %{randomNumber: :rand.uniform(10000)})
      Repo.update(changeset)
      w end))
  end

  def plaintext(conn, _params) do
    conn
    |> merge_resp_headers(%{"content-type" => "text/plain"})
    |> text("Hello, world!")
  end
end
