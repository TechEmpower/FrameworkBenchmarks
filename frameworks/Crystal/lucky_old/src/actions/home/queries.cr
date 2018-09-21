class Home::Queries < ApiAction
  get "/queries" do
    context.response.headers["Server"] = "Lucky"
    context.response.headers["Date"] = Time.utc_now.to_s("%a, %d %b %Y %H:%M:%S GMT")
    json({hello: "Hello World from Home::Queries"})
  end
end
