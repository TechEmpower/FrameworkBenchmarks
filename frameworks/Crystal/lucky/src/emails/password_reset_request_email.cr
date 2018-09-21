class PasswordResetRequestEmail < BaseEmail
  Habitat.create { setting stubbed_token : String? }
  delegate stubbed_token, to: :settings

  def initialize(@user : User)
    @token = stubbed_token || Authentic.generate_password_reset_token(@user)
  end

  to @user
  from "myapp@support.com" # or set a default in src/emails/base_email.cr
  subject "Reset your password"
  templates html, text
end
