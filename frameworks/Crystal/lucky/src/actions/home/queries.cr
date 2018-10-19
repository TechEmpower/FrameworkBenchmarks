class Home::Queries < ApiAction
  param queries : String = "1"

  get "/queries" do
    context.response.headers["Server"] = "Lucky"
    context.response.headers["Date"] = Time.utc_now.to_s("%a, %d %b %Y %H:%M:%S GMT")

    world_query = WorldQuery.new
    results = (1..queries.to_i.clamp(1..500)).map do
      id = rand(1..10_000)
      {id: id, randomNumber: world_query.random_world(id)}
    end

    results.to_json
  end
end
