defmodule FrameworkBenchmarksTest do
  use ExUnit.Case
  doctest FrameworkBenchmarks

  test "greets the world" do
    assert FrameworkBenchmarks.hello() == :world
  end
end
