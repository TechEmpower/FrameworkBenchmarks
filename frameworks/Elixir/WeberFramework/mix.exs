defmodule WeberFramework.Mixfile do
  use Mix.Project

  def project do
    [ 
      app: :WeberFramework,
      version: "0.0.1",
      deps: deps
    ]
  end

  def application do
    [
      applications: [],
      mod: {WeberFramework, []}
    ]
  end

  defp deps do
    [ 
      { :weber, github: "0xAX/weber" } 
    ]
  end
end
