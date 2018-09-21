class SignInForm < LuckyRecord::VirtualForm
  include Authentic::FormHelpers
  include FindAuthenticatable

  virtual email : String
  virtual password : String

  # This method is called to allow you to determine if a user can sign in.
  # By default it validates that the user exists and the password is correct.
  #
  # If desired, you can add additional checks in this method, e.g.
  #
  #    if user.locked?
  #      email.add_error "is locked out"
  #    end
  private def validate(user : User?)
    if user
      unless Authentic.correct_password?(user, password.value.to_s)
        password.add_error "is wrong"
      end
    else
      email.add_error "is not in our system"
    end
  end
end
