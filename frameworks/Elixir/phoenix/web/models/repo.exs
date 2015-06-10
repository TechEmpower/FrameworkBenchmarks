defmodule Hello.Repo do
  use Ecto.Repo, adapter: Ecto.Adapters.Postgres

  def conf do
    parse_url "ecto://username:password@host/database_name"
  end

  def priv do
    app_dir(:hello, "priv/repo")
  end
end
