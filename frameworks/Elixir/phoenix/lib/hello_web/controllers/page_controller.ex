defmodule HelloWeb.PageController do
  alias Hello.Models.{Fortune, World}

  use HelloWeb, :controller

  alias Hello.Repo
  alias Hello.Cache

  @json "application/json"
  @plain "text/plain"
  @random_max 10_000
  @all_ids 1..10_000

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
    :rand.seed(:exsp)

    resp =
      1..@random_max
      |> Enum.take_random(size(params["queries"]))
      |> parallel(fn idx -> Repo.get(World, idx) end)
      |> Jason.encode_to_iodata!()

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

    render(conn, :fortunes,
      fortunes: Enum.sort(fortunes, fn f1, f2 -> f1.message < f2.message end)
    )
  end

  def updates(conn, params) do
    :rand.seed(:exsp)

    count = size(params["queries"])

    worlds =
      1..@random_max
      |> Enum.take_random(count)
      |> parallel(fn idx -> Repo.get(World, idx) end)
      |> Enum.map(fn world ->
        %{id: world.id, randomnumber: random_but(world.randomnumber)}
      end)

    {^count, result} =
      Repo.insert_all(
        World,
        worlds,
        on_conflict: :replace_all,
        conflict_target: [:id],
        returning: true
      )

    conn
    |> put_resp_content_type(@json, nil)
    |> send_resp(200, Jason.encode_to_iodata!(result))
  end

  def plaintext(conn, _params) do
    conn
    |> put_resp_content_type(@plain, nil)
    |> send_resp(200, "Hello, world!")
  end

  def cached(conn, params) do
    :rand.seed(:exsp)

    resp =
      @all_ids
      |> Enum.take_random(size(params["count"]))
      |> parallel(&get_cached_world/1)
      |> Jason.encode_to_iodata!()

    conn
    |> put_resp_content_type(@json, nil)
    |> send_resp(200, resp)
  end

  defp get_cached_world(idx) do
    case Cache.get(idx) do
      nil ->
        world = Repo.get(World, idx)
        :ok = Cache.put(idx, world)
        world
      world ->
        world
    end
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
    |> Task.await_many()
  end

  defp size(nil), do: 1

  defp size(queries) do
    case Integer.parse(queries) do
      {x, ""} when x in 1..500 -> x
      {x, ""} when x > 500 -> 500
      _ -> 1
    end
  end
end
