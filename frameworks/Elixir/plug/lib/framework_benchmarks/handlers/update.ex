defmodule FrameworkBenchmarks.Handlers.Update do
  @moduledoc """
  handler for the /updates route
  """

  defp random_but(not_this_value) do
    case :rand.uniform(10_000) do
      new_value when new_value == not_this_value ->
        random_but(not_this_value)

      new_value ->
        new_value
    end
  end

  def handle(conn) do
    number_of_queries = FrameworkBenchmarks.Handlers.Helpers.parse_queries(conn, "queries")

    ids =
      1..number_of_queries
      |> Enum.map(fn _ ->
        :rand.uniform(10_000)
      end)

    records =
      ids
      |> Enum.map(
        &Task.async(fn ->
          %{randomnumber: current_randomnumber} =
            world = FrameworkBenchmarks.Repo.get(FrameworkBenchmarks.Models.World, &1)

          Ecto.Changeset.change(
            world,
            %{
              randomnumber: random_but(current_randomnumber)
            }
          )
          |> FrameworkBenchmarks.Repo.update!()
        end)
      )
      |> Enum.map(&Task.await(&1))

    {:ok, json} =
      records
      |> Enum.map(fn record ->
        record
        |> Map.from_struct()
        |> Map.drop([:__meta__])
      end)
      |> Eljiffy.encode()

    conn
    |> Plug.Conn.put_resp_content_type("application/json")
    |> Plug.Conn.send_resp(200, json)
  end
end
