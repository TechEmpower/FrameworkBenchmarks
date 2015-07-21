defmodule Hello.World do
  use Hello.Web, :model

  schema "world" do
    field :randomNumber, :integer
  end

  @required_fields ~w(randomNumber)
  @optional_fields ~w()

  def changeset(model, params \\ nil) do
    model
    |> cast(params, @required_fields, @optional_fields)
  end
end
