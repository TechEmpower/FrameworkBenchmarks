defmodule Hello.Models.Fortune do
  use Ecto.Schema

  @derive {Jason.Encoder, only: [:id, :message]}
  schema "fortune" do
    field :message, :string
  end
end
