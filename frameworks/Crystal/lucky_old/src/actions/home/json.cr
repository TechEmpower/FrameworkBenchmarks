class Home::Json < ApiAction
  get "/json" do
    context.response.headers["Server"] = "Lucky"
    context.response.headers["Date"] = Time.utc_now.to_s("%a, %d %b %Y %H:%M:%S GMT")
    json({message: "Hello, World!"})
  end
end
