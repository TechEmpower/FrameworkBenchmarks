Kemalyst::Handler::Session.config do |config|
  # The secret is used to avoid the session data being changed.  The session
  # data is stored in a cookie.  To avoid changes being made, a security token
  # is generated using this secret.  To generate a secret, you can use the
  # following command:
  # crystal eval "require \"secure_random\"; puts SecureRandom.hex(64)"
  #
  config.secret = "d3724601d0f9caf687320045dc05b29ae2aa7f9aa4a3b6bdb21d6dcdc4ffaf9f7d7c605c565f26dfafadcdb29875486300170076c033a0dfd62b220110e64f23"
end
