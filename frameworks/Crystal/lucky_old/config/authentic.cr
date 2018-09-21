require "./server"

Authentic.configure do
  settings.secret_key = Lucky::Server.settings.secret_key_base

  unless Lucky::Env.production?
    settings.encryption_cost = 4
  end
end
