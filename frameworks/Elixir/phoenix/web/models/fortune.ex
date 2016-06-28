defmodule Hello.Fortune do
  use Hello.Web, :model

  @derive {Poison.Encoder, only: [:id, :message]}
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
