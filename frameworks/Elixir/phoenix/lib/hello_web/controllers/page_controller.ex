defmodule HelloWeb.PageController do

  use HelloWeb, :controller

  alias Hello.Models.Fortune
  alias Hello.Models.World
  alias Hello.Repo
  alias Hello.WorldCache

  @random_max 10_000

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
    :rand.seed(:exsp)

    worlds =
      Stream.repeatedly(&random_id/0)
      |> Stream.uniq()
      |> Stream.map(&Repo.get(World, &1))
      |> Enum.take(size(params["queries"]))

    json(conn, worlds)
  end

  def fortunes(conn, _params) do
    additional_fortune = %Fortune{
      id: 0,
      message: "Additional fortune added at request time."
    }

    fortunes =
      [additional_fortune | Repo.all(Fortune)]
      |> Enum.sort_by(& &1.message)

    render(conn, :fortunes, fortunes: fortunes)
  end

  def updates(conn, params) do
    :rand.seed(:exsp)

    worlds =
      Stream.repeatedly(&random_id/0)
      |> Stream.uniq()
      |> Stream.map(&Repo.get(World, &1))
      |> Stream.map(fn world -> %{id: world.id, randomnumber: :rand.uniform(@random_max)} end)
      |> Enum.take(size(params["queries"]))
      # If this is not sorted it sometimes generates
      #  FAIL for http://tfb-server:8080/updates/20
      #  Only 20470 executed queries in the database out of roughly 20480 expected.
      |> Enum.sort_by(& &1.id)

    Repo.insert_all(
      World,
      worlds,
      on_conflict: {:replace_all_except, [:id]},
      conflict_target: [:id],
      returning: false
    )

    json(conn, worlds)
  end

  def plaintext(conn, _params) do
    text(conn, "Hello, World!")
  end

  def cached(conn, params) do
    :rand.seed(:exsp)
    WorldCache.seed()

    worlds =
      Stream.repeatedly(&random_id/0)
      |> Stream.uniq()
      |> Stream.map(&WorldCache.fetch(&1))
      |> Enum.take(size(params["count"]))

    json(conn, worlds)
  end

  defp random_id() do
    :rand.uniform(@random_max)
  end

  defp size(nil), do: 1
  defp size(""), do: 1

  defp size(queries) when is_bitstring(queries) do
    case Integer.parse(queries) do
      {count, _} -> max(1, min(500, count))
      _ -> 1
    end
  end

  defp size(_), do: 1
end
