defmodule HelloWeb.PageController do
  alias Hello.Models.{Fortune, World}

  use HelloWeb, :controller

  @json "application/json"
  @plain "text/plain"
  @random_max 10_000


  def index(conn, _params) do
    resp = Jason.encode!(%{"TE Benchmarks\n" => "Started"})

    conn
    |> put_resp_content_type(@json, nil)
    |> send_resp(200, resp)
  end

  # avoid namespace collision
  def _json(conn, _params) do
    resp = Jason.encode!(%{"message" => "Hello, world!"})

    conn
    |> put_resp_content_type(@json, nil)
    |> send_resp(200, resp)
  end

  def db(conn, _params) do
    resp =
      Repo.get(World, :rand.uniform(@random_max))
      |> Jason.encode!()

    conn
    |> put_resp_content_type(@json, nil)
    |> send_resp(200, resp)
  end

  def queries(conn, params) do
    resp =
      params["queries"]
      |> query_range()
      |> parallel(fn _ ->
        Repo.get(World, :rand.uniform(@random_max))
      end)
      |> Jason.encode!()

    conn
    |> put_resp_content_type(@json, nil)
    |> send_resp(200, resp)
  end

  def fortunes(conn, _params) do
    additional_fortune = %Fortune{
      id: 0,
      message: "Additional fortune added at request time."
    }

    fortunes = [additional_fortune | Repo.all(Fortune)]

    render(conn, "fortunes.html",
      fortunes: Enum.sort(fortunes, fn f1, f2 -> f1.message < f2.message end)
    )
  end

  def updates(conn, params) do
    resp =
      params["queries"]
      |> query_range()
      |> parallel(fn _ ->
        Repo.checkout(fn ->
          world =
            World
            |> Repo.get(:rand.uniform(@random_max))

          world
          |> Ecto.Changeset.change(randomnumber: random_but(world.randomnumber))
          |> Repo.update!()
        end)
      end)
      |> Jason.encode!()

    conn
    |> put_resp_content_type(@json, nil)
    |> send_resp(200, resp)
  end

  def plaintext(conn, _params) do
    conn
    |> put_resp_content_type(@plain, nil)
    |> send_resp(200, "Hello, world!")
  end

  defp random_but(not_this_value) do
    case :rand.uniform(@random_max) do
      new_value when new_value == not_this_value ->
        random_but(not_this_value)

      new_value ->
        new_value
    end
  end

  defp parallel(collection, func) do
    collection
    |> Enum.map(&Task.async(fn -> func.(&1) end))
    |> Enum.map(&Task.await(&1))
  end

  defp query_range(queries) do
    try do
      case String.to_integer(queries) do
        x when x < 1 -> 1..1
        x when x > 500 -> 1..500
        x -> 1..x
      end
    rescue
      ArgumentError -> 1..1
    end
  end
end
