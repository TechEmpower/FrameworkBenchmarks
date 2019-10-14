defmodule Hello.PageController do
  alias Hello.{Fortune, World}

  use Hello.Web, :controller

  @json "application/json"
  @plain "text/plain"

  def index(conn, _params) do
    conn
    |> put_resp_content_type(@json, nil)
    |> send_resp(200, Jason.encode_to_iodata!(%{"TE Benchmarks\n" => "Started"}))
  end

  # avoid namespace collision
  def _json(conn, _params) do
    conn
    |> put_resp_content_type(@json, nil)
    |> send_resp(200, Jason.encode_to_iodata!(%{"message" => "Hello, world!"}))
  end

  def db(conn, _params) do
    conn
    |> put_resp_content_type(@json, nil)
    |> send_resp(200, Jason.encode_to_iodata!(Repo.get(World, :rand.uniform(10000))))
  end

  def queries(conn, params) do
    json =
      params["queries"]
      |> query_range()
      |> parallel(fn _ -> Repo.get(World, :rand.uniform(10000)) end)
      |> Jason.encode_to_iodata!()

    conn
    |> put_resp_content_type(@json, nil)
    |> send_resp(200, json)
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
    json =
      params["queries"]
      |> query_range()
      |> parallel(fn _ ->
          Repo.checkout(fn ->
            World
            |> Repo.get(:rand.uniform(10000))
            |> Ecto.Changeset.change(randomnumber: :rand.uniform(10000))
            |> Repo.update!()
          end)
        end)
      |> Jason.encode_to_iodata!()

    conn
    |> put_resp_content_type(@json, nil)
    |> send_resp(200, json)
  end

  def plaintext(conn, _params) do
    conn
    |> put_resp_content_type(@plain, nil)
    |> send_resp(200, "Hello, world!")
  end

  defp parallel(collection, func) do
    collection
    |> Enum.map(&Task.async(fn -> func.(&1) end))
    |> Enum.map(&Task.await(&1))
  end

  defp query_range(queries) do
    try do
      case String.to_integer(queries) do
        x when x < 1    -> 1..1
        x when x > 500  -> 1..500
        x               -> 1..x
      end
    rescue
      ArgumentError -> 1..1
    end
  end
end
