BaseEmail.configure do
  if Lucky::Env.production?
    # If you don't need to send emails, set the adapter to DevAdapter instead:
    #
    #   settings.adapter = Carbon::DevAdapter.new
    #
    # If you do need emails, get a key from SendGrid and set an ENV variable
    send_grid_key = send_grid_key_from_env
    settings.adapter = Carbon::SendGridAdapter.new(api_key: send_grid_key)
  else
    settings.adapter = Carbon::DevAdapter.new
  end
end

private def send_grid_key_from_env
  ENV["SEND_GRID_KEY"]? || raise_missing_key_message
end

private def raise_missing_key_message
  raise "If you are sending emails, get a key from SendGrid and set the SEND_GRID_KEY environment variable. If you are not sending email, use the Carbon::DevAdapter in 'config/email.cr'"
end
