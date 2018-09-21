class PasswordResetRequests::NewPage < GuestLayout
  needs form : PasswordResetRequestForm

  def content
    h1 "Reset your password"
    render_form(@form)
  end

  private def render_form(f)
    form_for PasswordResetRequests::Create do
      field(f.email) { |i| email_input i }
      submit "Reset Password", flow_id: "request-password-reset-button"
    end
  end
end
