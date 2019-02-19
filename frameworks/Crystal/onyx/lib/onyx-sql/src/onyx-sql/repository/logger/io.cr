require "time_format"
require "colorize"

require "../logger"

# Logs anything followed by time elapsed by the block run into the specified `IO`.
#
# ```
# logger = Onyx::SQL::Repository::Logger::IO.new(STDOUT)
#
# result = logger.wrap("SELECT * FROM users") do
#   db.query("SELECT * FROM users")
# end
#
# # => SELECT * FROM users
# # => 501μs
# ```
class Onyx::SQL::Repository::Logger::IO < Onyx::SQL::Repository::Logger
  def initialize(@io : ::IO, @colors = true)
  end

  # Wrap a block, logging elapsed time and returning the result.
  def wrap(data_to_log : String, &block)
    log(data_to_log)
    started_at = Time.monotonic

    result = yield

    log_elapsed(TimeFormat.auto(Time.monotonic - started_at))
    result
  end

  protected def log(data_to_log)
    @io << (@colors ? data_to_log.colorize(:blue).to_s : data_to_log) + "\n"
  end

  protected def log_elapsed(time)
    @io << (@colors ? time.colorize(:magenta).to_s : time) + "\n"
  end
end
