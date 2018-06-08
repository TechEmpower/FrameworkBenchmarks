defmodule Hello.World do
  use Hello.Web, :model

  @derive {Jason.Encoder, only: [:id, :randomnumber]}
  schema "world" do
    field :randomnumber, :integer
  end
end
