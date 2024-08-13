defmodule Hello.WorldCache do
  use Nebulex.Cache,
    otp_app: :hello,
    adapter: Nebulex.Adapters.Local

  alias Hello.Models.World
  alias Hello.Repo

  def seed do
    if not __MODULE__.has_key?(:seeded) do
      World
      |> Repo.all()
      |> Enum.into([], &{&1.id, &1})
      |> __MODULE__.put_all()

      __MODULE__.put(:seeded, true)
    end
  end

  def fetch(id) do
    case __MODULE__.get(id) do
      nil ->
        world = Repo.get(World, id)
        :ok = __MODULE__.put(id, world)
        world
      world ->
        world
    end
  end

end
