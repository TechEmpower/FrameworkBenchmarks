Lucky::LogHandler.configure do
  settings.show_timestamps = Lucky::Env.production?
end
