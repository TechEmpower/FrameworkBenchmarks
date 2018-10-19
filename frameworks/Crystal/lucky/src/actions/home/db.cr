class Home::Db < ApiAction
  get "/db" do
    context.response.headers["Server"] = "Lucky"
    context.response.headers["Date"] = Time.utc_now.to_s("%a, %d %b %Y %H:%M:%S GMT")
    id = rand(1..10_000)
    { id: id, randomNumber: WorldQuery.new.random_world(id) }
  end
end
