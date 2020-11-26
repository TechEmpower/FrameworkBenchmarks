class Home::Index < BaseAction
  get "/" do
    html Lucky::WelcomePage
  end
end
