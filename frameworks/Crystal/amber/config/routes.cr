Amber::Server.instance.config do |app|
  routes :web do
    get "/plaintext", BenchmarkController, :plaintext
    get "/json", BenchmarkController, :json
    get "/db", BenchmarkController, :db
    get "/queries", BenchmarkController, :queries
    get "/updates", BenchmarkController, :updates
    get "/fortunes", BenchmarkController, :fortunes
  end
end
