defmodule FrameworkBenchmarks.Handlers.Fortune do
  @moduledoc """
  This is the handle for the /fortunes route
  """

  require EEx

  @fortune_template """
  <!DOCTYPE html>
  <html>
  <head><title>Fortunes</title></head>
  <body>
    <table>
      <tr><th>id</th><th>message</th></tr>
      <%= for item <- items do %>
      <tr><td><%= item.id %></td><td><%= item.message %></td></tr>
      <% end %>
    </table>
  </body>
  </html>
  """

  EEx.function_from_string(:defp, :generate_fortunes, @fortune_template, [:items])

  @new_message "Additional fortune added at request time."

  def handle(conn) do
    fortunes =
      FrameworkBenchmarks.Repo.all(FrameworkBenchmarks.Models.Fortune)
      |> Enum.map(fn fortune ->
        safe_result = Phoenix.HTML.html_escape(fortune.message)
        %{id: fortune.id, message: Phoenix.HTML.safe_to_string(safe_result)}
      end)

    fortunes = [%{id: 0, message: @new_message}] ++ fortunes

    fortunes =
      fortunes
      |> Enum.sort(fn f1, f2 -> f1.message < f2.message end)

    conn
    |> Plug.Conn.put_resp_content_type("text/html")
    |> Plug.Conn.send_resp(200, generate_fortunes(fortunes))
  end
end
