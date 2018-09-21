module Auth::PasswordResets::Base
  macro included
    include Auth::RedirectIfSignedIn
    include Auth::PasswordResets::FindUser
    include Auth::PasswordResets::RequireToken
  end
end
