require "../logger"

# Does not log anything.
class Onyx::SQL::Repository::Logger::Dummy < Onyx::SQL::Repository::Logger
  # Does nothing except yielding the *block*.
  def wrap(data_to_log : String, &block)
    yield
  end
end
