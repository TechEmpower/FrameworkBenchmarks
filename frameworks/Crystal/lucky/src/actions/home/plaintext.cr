class Home::Plaintext < ApiAction
  get "/plaintext" do
    context.response.headers["Server"] = "Lucky"
    context.response.headers["Date"] = Time.utc_now.to_s("%a, %d %b %Y %H:%M:%S GMT")
    text("Hello, World!")
  end
end
