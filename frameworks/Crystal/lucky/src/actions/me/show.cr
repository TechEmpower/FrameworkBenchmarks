class Me::Show < BrowserAction
  get "/me" do
    render ShowPage
  end
end
