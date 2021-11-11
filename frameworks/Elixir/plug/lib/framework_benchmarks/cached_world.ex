defmodule FrameworkBenchmarks.CachedWorld do
  @moduledoc """
  Cache for the CachedWorld table
  """

  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok,
      name: {:via, Registry, {FrameworkBenchmarks.Registry, __MODULE__}}
    )
  end

  def get(id) do
    GenServer.call({:via, Registry, {FrameworkBenchmarks.Registry, __MODULE__}}, {:get, id})
  end

  @impl true
  def init(_) do
    Cachex.start_link(:cached_world, [])

    records =
      FrameworkBenchmarks.Repo.all(FrameworkBenchmarks.Models.World)
      |> Enum.map(fn record ->
        {
          Map.get(record, :id),
          record
          |> Map.from_struct()
          |> Map.drop([:__meta__])
        }
      end)

    Cachex.put_many(:cached_world, records)
  end

  @impl true
  def handle_call({:get, id}, _from, state) do
    record =
      case Cachex.get(:cached_world, id) do
        {:ok, nil} ->
          FrameworkBenchmarks.Repo.get(FrameworkBenchmarks.Models.World, id)
          |> Map.from_struct()
          |> Map.drop([:__meta__])

        {:ok, value} ->
          value
      end

    {:reply, record, state}
  end
end
