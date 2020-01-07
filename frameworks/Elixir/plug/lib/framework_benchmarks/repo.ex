defmodule FrameworkBenchmarks.Repo do
  use Ecto.Repo,
    otp_app: :framework_benchmarks,
    adapter: Ecto.Adapters.Postgres
end
