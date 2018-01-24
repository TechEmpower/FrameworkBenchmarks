Amber::Server.configure do |app|
  pipeline :web do
  end

  routes :web do
    get "/db", BenchmarkController, :db
    get "/json", BenchmarkController, :json
    get "/queries", BenchmarkController, :queries
    get "/updates", BenchmarkController, :updates
    get "/fortunes", BenchmarkController, :fortunes
    get "/plaintext", BenchmarkController, :plaintext
  end
end
