defmodule FrameworkBenchmarks.Handlers.Helpers do
  @moduledoc """
  Helper functions for shared tasks between routes
  """

  @doc """
  Used to parse the number value from the query string between 1 and 500
  """
  def parse_queries(conn, name) do
    with %{^name => count} <- Plug.Conn.fetch_query_params(conn).query_params do
      case Integer.parse(count) do
        {num, _} when num >= 1 and num <= 500 -> num
        {num, _} when num < 1 -> 1
        {num, _} when num > 500 -> 500
        _ -> 1
      end
    else
      _ -> 1
    end
  end
end
