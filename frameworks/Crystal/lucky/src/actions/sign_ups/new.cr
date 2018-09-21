class SignUps::New < BrowserAction
  include Auth::RedirectIfSignedIn

  get "/sign_up" do
    render NewPage, form: SignUpForm.new
  end
end
