module Auth::RedirectIfSignedIn
  macro included
    include Auth::SkipRequireSignIn
    before redirect_if_signed_in
    unexpose current_user
  end

  private def redirect_if_signed_in
    if current_user?
      flash.success = "You are already signed in"
      redirect to: Home::Index
    else
      continue
    end
  end

  # current_user returns nil so that it won't accidentally be used
  def current_user
  end
end
