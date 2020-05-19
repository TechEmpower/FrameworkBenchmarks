defmodule FrameworkBenchmarks.Models.Fortune do
  use Ecto.Schema

  schema "fortune" do
    field(:message, :binary)
  end
end
