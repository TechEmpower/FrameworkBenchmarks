Rage.routes.draw do
  root to: ->(env) { [200, {}, "It works!"] }

  get "json", to: "benchmarks#json"
  get "plaintext", to: "benchmarks#plaintext"
  get "db", to: "benchmarks#db"
  get "queries", to: "benchmarks#queries"
  get "fortunes", to: "benchmarks#fortunes"
  get "updates", to: "benchmarks#updates"
end
