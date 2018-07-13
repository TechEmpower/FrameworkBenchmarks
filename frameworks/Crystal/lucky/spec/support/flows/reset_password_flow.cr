class ResetPasswordFlow < BaseFlow
  private getter user, authentication_flow
  delegate sign_in, sign_out, should_have_password_error, should_be_signed_in,
    to: authentication_flow
  delegate email, to: user

  def initialize(@user : User)
    @authentication_flow = AuthenticationFlow.new(user.email)
  end

  def request_password_reset
    with_fake_token do
      visit PasswordResetRequests::New
      fill_form PasswordResetRequestForm,
        email: email
      click "@request-password-reset-button"
    end
  end

  def should_have_sent_reset_email
    with_fake_token do
      user = UserQuery.new.email(email).first
      PasswordResetRequestEmail.new(user).should be_delivered
    end
  end

  def reset_password(password)
    user = UserQuery.new.email(email).first
    token = Authentic.generate_password_reset_token(user)
    visit PasswordResets::New.with(user.id, token)
    fill_form PasswordResetForm,
      password: password,
      password_confirmation: password
    click "@update-password-button"
  end

  private def with_fake_token
    PasswordResetRequestEmail.temp_config(stubbed_token: "fake") do
      yield
    end
  end
end
