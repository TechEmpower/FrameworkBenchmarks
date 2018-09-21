module Auth::SkipRequireSignIn
  def require_sign_in
    continue
  end

  # Since sign in is not required, current_user might be nil
  def current_user : User?
    current_user?
  end
end
