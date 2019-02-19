require "logger"
require "colorize"
require "time_format"

require "../logger"

# Logs anything followed by elapsed time by the block into a standard `Logger`.
#
# ```
# logger = Logger.new(STDOUT, Logger::Severity::INFO)
# repo_logger = Onyx::SQL::Repository::Logger::Standard.new(logger)
#
# result = repo_logger.wrap("SELECT * FROM users") do
#   db.query("SELECT * FROM users")
# end
#
# # [21:54:51:068]  INFO > SELECT * FROM users
# # [21:54:51:068]  INFO > 501μs
# ```
class Onyx::SQL::Repository::Logger::Standard < Onyx::SQL::Repository::Logger
  def initialize(
    @logger : ::Logger = ::Logger.new(STDOUT, ::Logger::Severity::INFO),
    @log_level : ::Logger::Severity = ::Logger::Severity::INFO,
    @colors = true
  )
  end

  # Wrap a block, logging elapsed time at *log_level* and returning the result.
  def wrap(data_to_log : String, &block)
    log(data_to_log)
    started_at = Time.monotonic

    result = yield

    log_elapsed(TimeFormat.auto(Time.monotonic - started_at))

    result
  end

  protected def log(data_to_log)
    @logger.log(@log_level, @colors ? data_to_log.colorize(:blue) : data_to_log)
  end

  protected def log_elapsed(time)
    @logger.log(@log_level, @colors ? time.colorize(:magenta) : time)
  end
end
