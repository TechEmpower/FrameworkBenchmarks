defmodule HelloWeb do
  @moduledoc """
  The entrypoint for defining your web interface, such
  as controllers, views, channels and so on.

  This can be used in your application as:

      use HelloWeb, :controller
      use HelloWeb, :html

  The definitions below will be executed for every controller,
  component, etc, so keep them short and clean, focused
  on imports, uses and aliases.

  Do NOT define functions inside the quoted expressions
  below. Instead, define additional modules and import
  those modules here.
  """

  def static_paths,
    do: ~w(assets favicon.svg apple-touch-icon.png robots.txt font-files mask-icon.svg)

  def controller do
    quote do
      use Phoenix.Controller,
        namespace: HelloWeb,
        formats: [:html, :json],
        layouts: [html: HelloWeb.Layouts],
        log: false

      import Plug.Conn
      import HelloWeb.Gettext

      unquote(verified_routes())
    end
  end

  def component do
    quote do
      use Phoenix.Component

      import HelloWeb.Gettext

      # Routes generation with the ~p sigil
      unquote(verified_routes())
    end
  end

  def html do
    quote do
      use Phoenix.Component

      # Import convenience functions from controllers
      import Phoenix.Controller,
        only: [get_csrf_token: 0, view_module: 1, view_template: 1]

      # Include general helpers for rendering HTML
      unquote(html_helpers())
    end
  end

  defp html_helpers do
    quote do
      # Use all HTML functionality (forms, tags, etc)
      use Phoenix.HTML
      # Core UI Components and translation
      import HelloWeb.Gettext

      # Routes generation with the ~p sigil
      unquote(verified_routes())
    end
  end

  def verified_routes do
    quote do
      use Phoenix.VerifiedRoutes,
        endpoint: HelloWeb.Endpoint,
        router: HelloWeb.Router,
        statics: HelloWeb.static_paths()
    end
  end

  def router do
    quote do
      use Phoenix.Router, helpers: false

      # Import common connection and controller functions to use in pipelines
      import Plug.Conn
      import Phoenix.Controller
    end
  end

  def channel do
    quote do
      use Phoenix.Channel
    end
  end

  @doc """
    When used, dispatch to the appropriate controller/view/etc.
    """
  defmacro __using__(which) when is_atom(which) do
    apply(__MODULE__, which, [])
  end
end
