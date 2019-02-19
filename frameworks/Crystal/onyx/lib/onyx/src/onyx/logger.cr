require "logger"
require "./utils/require_env"
require "./utils/custom_log_formatter"

class Onyx
  # The top-level `Logger` instance. Has a custom slim formatter and
  # level depending on `CRYSTAL_ENV` environment variable:
  #
  # * `"production"` -- `INFO`
  # * all other (including `nil`) -- `DEBUG`
  #
  # ```
  # require "onyx/logger"
  # Onyx.logger.debug("Hello world!")
  # # DEBUG [12:45:52.520 #13543] Hello world!
  # ```
  class_property logger : Logger = Logger.new(
    STDOUT,
    ENV["CRYSTAL_ENV"]? == "production" ? Logger::INFO : Logger::DEBUG,
    formatter: custom_log_formatter,
  )
end
