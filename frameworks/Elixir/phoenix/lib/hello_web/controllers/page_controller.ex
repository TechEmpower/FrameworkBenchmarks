defmodule HelloWeb.PageController do
  use HelloWeb, :controller

  alias Hello.Models.Fortune
  alias Hello.Models.World
  alias Hello.Repo
  alias Hello.WorldCache

  @random_max 10_000

  plug :accepts, ~w(html json) when action == :fortunes

  def index(conn, _params) do
    json(conn, %{"TE Benchmarks\n" => "Started"})
  end

  # avoid namespace collision
  def _json(conn, _params) do
    json(conn, %{message: "Hello, World!"})
  end

  def db(conn, _params) do
    world = Repo.get(World, random_id())

    json(conn, world)
  end

  def queries(conn, params) do
    worlds =
      Repo.checkout(fn ->
        params["queries"]
        |> random_ids_sample()
        |> Enum.map(&Repo.get(World, &1))
      end)

    json(conn, worlds)
  end

  def fortunes(conn, _params) do
    additional_fortune = %Fortune{
      id: 0,
      message: "Additional fortune added at request time."
    }

    fortunes =
      [additional_fortune | Repo.all(Fortune)]
      |> Enum.sort(fn a, b -> a.message < b.message end)

    render(conn, :fortunes, fortunes: fortunes)
  end

  def updates(conn, params) do
    world_updates =
      Repo.checkout(fn ->
        params["queries"]
        |> random_ids_sample()
        |> Enum.sort()
        #
        # If this is not sorted it will intermittently generate:
        #
        #   FAIL for http://tfb-server:8080/updates/20
        #   Only 20470 executed queries in the database out of roughly 20480 expected.
        #
        |> Enum.map(fn id ->
          world = Repo.get(World, id)
          %{id: world.id, randomnumber: :rand.uniform(@random_max)}
        end)
      end)

    Repo.insert_all(
      World,
      world_updates,
      on_conflict: {:replace_all_except, [:id]},
      conflict_target: [:id],
      returning: false
    )

    json(conn, world_updates)
  end

  def plaintext(conn, _params) do
    conn
    |> put_resp_header("content-type", "text/plain")
    |> send_resp(200, "Hello, World!")
  end

  def cached(conn, params) do
    WorldCache.seed()

    worlds =
      params["count"]
      |> random_ids_sample()
      |> Enum.map(&WorldCache.fetch(&1))

    json(conn, worlds)
  end

  defp random_id() do
    :rand.uniform(@random_max)
  end

  defp random_ids_sample(count) do
    # Use the fastest rand algorithm
    :rand.seed(:exsp)

    Stream.repeatedly(&random_id/0)
    |> Stream.uniq()
    |> Enum.take(size(count))
  end

  defp size(param_count) when is_bitstring(param_count) do
    case Integer.parse(param_count) do
      {count, _} -> max(1, min(500, count))
      _ -> 1
    end
  end

  defp size(_), do: 1
end
