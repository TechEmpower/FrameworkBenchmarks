defmodule HelloWeb do
  @moduledoc """
  A module that keeps using definitions for controllers,
  views and so on.

  This can be used in your application as:

      use HelloWeb, :controller
      use HelloWeb, :view

  The definitions below will be executed for every view,
  controller, etc, so keep them short and clean, focused
  on imports, uses and aliases.

  Do NOT define functions inside the quoted expressions
  below. Instead, define any helper function in modules
  and import those modules here.
  """

  def controller do
    quote do
      use Phoenix.Controller, namespace: HelloWeb, log: false

      # Alias the data repository and import query/model functions
      alias Hello.Repo
      import Ecto
      import Ecto.Query

      # Import URL helpers from the router
      import HelloWeb.Router.Helpers
    end
  end

  def view do
    quote do
      use Phoenix.View, 
        root: "lib/hello_web/templates",
        namespace: HelloWeb

      # Import convenience functions from controllers
      import Phoenix.Controller,
        only: [get_csrf_token: 0, get_flash: 2, action_name: 1, controller_module: 1]

      # Import all HTML functions (forms, tags, etc)
      use Phoenix.HTML

      import HelloWeb.Router.Helpers
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
      import Ecto
      import Ecto.Query
    end
  end

  @doc """
  When used, dispatch to the appropriate controller/view/etc.
  """
  defmacro __using__(which) when is_atom(which) do
    apply(__MODULE__, which, [])
  end
end
