class PasswordResetRequests::Create < BrowserAction
  include Auth::RedirectIfSignedIn

  route do
    PasswordResetRequestForm.new(params).submit do |form, user|
      if user
        PasswordResetRequestEmail.new(user).deliver
        flash.success = "You should receive an email on how to reset your password shortly"
        redirect SignIns::New
      else
        render NewPage, form: form
      end
    end
  end
end
