defmodule Hello.PageView do
  use Hello.Web, :view
  @fortune_attributes [:id, :message]
  @world_attributes [:id, :randomnumber]

  def render("db.json", %{data: world}) do
    world
    |> Map.take(@world_attributes)
  end

  def render("queries.json", %{data: worlds}) do
    for world <- worlds do
      render("db.json", data: world)
    end
  end


end
