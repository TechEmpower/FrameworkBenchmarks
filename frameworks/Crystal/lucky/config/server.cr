# Here is where you configure the Lucky server
#
# Look at config/route_helper.cr if you want to change the domain used when
# generating links with `Action.url`.
Lucky::Server.configure do |settings|
  settings.secret_key_base = "u4PWnhZfOFXdTOtoiSBF+6jn0zHbYS6/yumo3WXYNSw"
  settings.host = "0.0.0.0"
  settings.port = 8080
  settings.gzip_enabled = true
  settings.asset_host = ""
end

Lucky::ForceSSLHandler.configure do |settings|
  settings.enabled = false
end
