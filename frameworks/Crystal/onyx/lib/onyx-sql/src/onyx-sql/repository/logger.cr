module Onyx::SQL
  class Repository
    # Logs anything with time elapsed (presumably requests from `Repository`).
    abstract class Logger
      abstract def wrap(data_to_log : String, &block)
    end
  end
end

require "./logger/*"
