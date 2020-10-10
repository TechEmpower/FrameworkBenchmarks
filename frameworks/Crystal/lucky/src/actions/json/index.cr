class Json::Index < BaseAction
  get "/json" do
    json({message: "Hello, World!"})
  end
end
