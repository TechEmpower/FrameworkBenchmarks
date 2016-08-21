defmodule Hello.World do
  use Hello.Web, :model

  @derive {Poison.Encoder, only: [:id, :randomnumber]}
  schema "world" do
    field :randomnumber, :integer
  end

  @required_fields ~w(randomnumber)a
  @optional_fields ~w()

  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields)
  end
end
