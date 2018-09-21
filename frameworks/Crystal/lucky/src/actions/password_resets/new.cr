class PasswordResets::New < BrowserAction
  include Auth::PasswordResets::Base

  param token : String

  get "/password_resets/:user_id" do
    redirect_to_edit_form_without_token_param
  end

  # This is to prevent password reset tokens from being scraped in the HTTP Referer header
  # See more info here: https://github.com/thoughtbot/clearance/pull/707
  private def redirect_to_edit_form_without_token_param
    make_token_available_to_future_actions
    redirect to: PasswordResets::Edit.with(user_id)
  end

  private def make_token_available_to_future_actions
    session[:password_reset_token] = token
  end
end
