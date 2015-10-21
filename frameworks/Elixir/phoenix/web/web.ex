defmodule Hello.Web do
  @moduledoc """
  A module that keeps using definitions for controllers,
  views and so on.

  This can be used in your application as:

      use Hello.Web, :controller
      use Hello.Web, :view

  Keep the definitions in this module short and clean,
  mostly focused on imports, uses and aliases.
  """

  def model do
    quote do
      use Ecto.Model
    end
  end

  def controller do
    quote do
      use Phoenix.Controller

      # Alias the data repository and import query/model functions
      alias Hello.Repo
      import Ecto.Model
      import Ecto.Query

      # Import URL helpers from the router
      import Hello.Router.Helpers
    end
  end

  def view do
    quote do
      use Phoenix.View, root: "web/templates"

      # Import convenience functions from controllers
      import Phoenix.Controller, only: [get_csrf_token: 0, get_flash: 2,
                                        action_name: 1, controller_module: 1]

      # Import URL helpers from the router
      import Hello.Router.Helpers

      # Import all HTML functions (forms, tags, etc)
      use Phoenix.HTML
    end
  end

  def router do
    quote do
      use Phoenix.Router
    end
  end


  def channel do
    quote do
      use Phoenix.Channel
      # Alias the data repository and import query/model functions
      alias Hello.Repo
      import Ecto.Model
      import Ecto.Query, only: [from: 2]
    end
  end

  @doc """
  When used, dispatch to the appropriate controller/view/etc.
  """
  defmacro __using__(which) when is_atom(which) do
    apply(__MODULE__, which, [])
  end
end
