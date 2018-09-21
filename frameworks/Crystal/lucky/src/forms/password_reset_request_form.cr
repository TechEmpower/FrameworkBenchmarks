class PasswordResetRequestForm < LuckyRecord::VirtualForm
  include Authentic::FormHelpers
  include FindAuthenticatable

  virtual email : String

  def validate(user : User?)
    validate_required email
    if user.nil?
      email.add_error "is not in our system"
    end
  end
end
