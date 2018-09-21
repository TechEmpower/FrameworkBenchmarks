class PasswordResetForm < User::BaseForm
  # Change password validations in src/forms/mixins/password_validations.cr
  include PasswordValidations

  virtual password : String
  virtual password_confirmation : String

  def prepare
    run_password_validations
    Authentic.copy_and_encrypt password, to: encrypted_password
  end
end
