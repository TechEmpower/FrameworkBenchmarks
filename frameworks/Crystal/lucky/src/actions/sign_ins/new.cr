class SignIns::New < BrowserAction
  include Auth::RedirectIfSignedIn

  get "/sign_in" do
    render NewPage, form: SignInForm.new
  end
end
