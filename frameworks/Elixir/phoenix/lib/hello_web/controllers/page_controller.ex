defmodule HelloWeb.PageController do
  alias Hello.Models.{Fortune, World}

  use HelloWeb, :controller

  alias Hello.Repo
  alias Hello.Cache

  @random_max 10_000

  def index(conn, _params) do
    json(conn, %{"TE Benchmarks\n" => "Started"})
  end

  # avoid namespace collision
  def _json(conn, _params) do
    json(conn, %{message: "Hello, World!"})
  end

  def db(conn, _params) do
    world = Repo.get(World, :rand.uniform(@random_max))

    json(conn, world)
  end

  def queries(conn, params) do
    :rand.seed(:exsp)

    worlds =
      Stream.repeatedly(fn -> :rand.uniform(@random_max) end)
      |> Stream.uniq()
      |> Stream.map(fn idx -> Repo.get(World, idx) end)
      |> Enum.take(size(params["queries"]))

    json(conn, worlds)
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

    worlds =
      Stream.repeatedly(fn -> :rand.uniform(@random_max) end)
      |> Stream.uniq()
      |> Stream.map(fn idx -> Repo.get(World, idx) end)
      |> Stream.map(fn world -> %{id: world.id, randomnumber: :rand.uniform(@random_max)} end)
      |> Enum.take(size(params["queries"]))

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

    worlds =
      Stream.repeatedly(fn -> :rand.uniform(@random_max) end)
      |> Stream.uniq()
      |> Stream.map(&get_cached_world/1)
      |> Enum.take(size(params["count"]))

    json(conn, worlds)
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
