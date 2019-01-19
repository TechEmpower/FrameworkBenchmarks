defmodule Hello.PageController do
  use Hello.Web, :controller
  alias Hello.World
  alias Hello.Fortune

  def index(conn, _params) do
    conn
    |> put_resp_content_type("application/json", nil)
    |> send_resp(200, Jason.encode_to_iodata!(%{"TE Benchmarks\n" => "Started"}))
  end

  # avoid namespace collision
  def _json(conn, _params) do
    conn
    |> put_resp_content_type("application/json", nil)
    |> send_resp(200, Jason.encode_to_iodata!(%{"message" => "Hello, world!"}))
  end

  def db(conn, _params) do
    conn
    |> put_resp_content_type("application/json", nil)
    |> send_resp(200, Jason.encode_to_iodata!(Repo.get(World, :rand.uniform(10000))))
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
    |> put_resp_content_type("application/json", nil)
    |> send_resp(200, Jason.encode_to_iodata!(for _ <- 1..q, do: Repo.get(World, :rand.uniform(10000))))
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

    data = for _ <- 1..q do
      id = :rand.uniform(10000)
      num = :rand.uniform(10000)
      w = Repo.get(World, id)
      changeset = Ecto.Changeset.change(w, randomnumber: num)
      Repo.update!(changeset)
    end

    conn
    |> put_resp_content_type("application/json", nil)
    |> send_resp(200, Jason.encode_to_iodata!(data))
  end

  def plaintext(conn, _params) do
    conn
    |> put_resp_content_type("text/plain", nil)
    |> send_resp(200, "Hello, world!")
  end
end
