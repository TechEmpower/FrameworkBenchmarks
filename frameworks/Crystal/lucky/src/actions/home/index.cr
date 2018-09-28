class Home::Index < BrowserAction
  get "/" do
    context.response.headers["Server"] = "Lucky"
    context.response.headers["Date"] = Time.utc_now.to_s("%a, %d %b %Y %H:%M:%S GMT")

    id = Random.rand(10_000).succ
    # random_number = BENCH_DB.query_one("SELECT randomNumber FROM world WHERE id = $1", id, as: Int32)
    { id: id, randomNumber: random_number }

    json({hello: "Hello World from Home::Fortunes"})
  end
end
