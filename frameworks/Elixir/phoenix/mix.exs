defmodule Hello.Mixfile do
  use Mix.Project

  def project do
   [app: :hello,
    version: "0.0.1",
    elixir: "~> 1.0",
    elixirc_paths: elixirc_paths(Mix.env),
    compilers: [:phoenix] ++ Mix.compilers,
    build_embedded: Mix.env == :prod,
    start_permanent: Mix.env == :prod,
    deps: deps]
  end

  # Configuration for the OTP application
  #
  # Type `mix help compile.app` for more information
  def application do
    [mod: {Hello, []},
     applications: [:phoenix, :phoenix_ecto, :mariaex, :cowboy, :logger, :phoenix_html]]
  end

  defp elixirc_paths(_), do: ["lib", "web"]

  # Specifies your project dependencies
  #
  # Type `mix help deps` for examples and options
  defp deps do
    [{:phoenix, "~> 0.13.1"},
     {:phoenix_ecto, "~> 0.4"},
     {:mariaex, "~> 0.3.0"},
     {:cowboy, "~> 1.0"},
     {:phoenix_html, "~> 1.0"},
     {:phoenix_live_reload, "~> 0.4", only: :dev},
     {:exrm, "~> 0.15.3"}]
  end
end
