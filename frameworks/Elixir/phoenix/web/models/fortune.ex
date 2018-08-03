defmodule Hello.Fortune do
  use Hello.Web, :model

  @derive {Jason.Encoder, only: [:id, :message]}
  schema "fortune" do
    field :message, :string
  end
end
