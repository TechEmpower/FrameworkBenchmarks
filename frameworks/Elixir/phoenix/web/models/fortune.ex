defmodule Hello.Fortune do
  use Hello.Web, :model

  schema "fortune" do
    field :message, :string
  end

  @required_fields ~w(message)
  @optional_fields ~w()

  def changeset(model, params \\ nil) do
    model
    |> cast(params, @required_fields, @optional_fields)
  end
end
