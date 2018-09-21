class PasswordResetRequests::New < BrowserAction
  include Auth::RedirectIfSignedIn

  route do
    render NewPage, form: PasswordResetRequestForm.new
  end
end
