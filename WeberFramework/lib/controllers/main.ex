defmodule WeberFramework.Main do
  
  use Weber.Controller

  def action_json(_, _) do
    {:json, [message: "Hello, world!"], [{"Content-type", "application/json"}]}
  end

  def action_text(_, _) do
  	{:text, "Hello, world!", [{"Content-type", "text/plain"}]}
  end

end
