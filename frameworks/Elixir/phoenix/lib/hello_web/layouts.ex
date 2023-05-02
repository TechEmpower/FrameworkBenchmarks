defmodule HelloWeb.Layouts do
  use HelloWeb, :html

  @moduledoc false

  embed_templates "layouts/*"
end
