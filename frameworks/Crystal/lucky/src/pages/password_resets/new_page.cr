class PasswordResets::NewPage < GuestLayout
  needs form : PasswordResetForm
  needs user_id : Int32

  def content
    h1 "Reset your password"
    render_password_reset_form(@form)
  end

  private def render_password_reset_form(f)
    form_for PasswordResets::Create.with(@user_id) do
      field(f.password) { |i| password_input i, autofocus: "true" }
      field(f.password_confirmation) { |i| password_input i }

      submit "Update Password", flow_id: "update-password-button"
    end
  end
end
