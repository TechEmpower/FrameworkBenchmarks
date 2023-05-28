defmodule Hello.Models.World do
  use Ecto.Schema

  import Ecto.Changeset

  @derive {Jason.Encoder, only: [:id, :randomnumber]}
  schema "world" do
    field :randomnumber, :integer
  end

  def changeset(world, params \\ %{}) do
    world
    |> cast(params, [:randomnumber])
  end
end
