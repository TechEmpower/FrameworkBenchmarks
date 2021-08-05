defmodule Hello.Models.World do
  use Ecto.Schema

  @derive {Jason.Encoder, only: [:id, :randomnumber]}
  schema "world" do
    field :randomnumber, :integer
  end
end
