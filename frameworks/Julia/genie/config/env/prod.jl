using Genie, Logging

Genie.Configuration.config!(
  server_port                     = 8080,
  server_host                     = "0.0.0.0",
  log_level                       = Logging.Info,
  log_to_file                     = false,
  server_handle_static_files      = false,
  path_build                      = "build",
  format_julia_builds             = false,
  format_html_output              = false
)

ENV["JULIA_REVISE"] = "off"