Lucky::ErrorHandler.configure do
  settings.show_debug_output = !Lucky::Env.production?
end
