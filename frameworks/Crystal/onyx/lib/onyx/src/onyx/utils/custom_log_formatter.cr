require "logger"
require "colorize"

# A custom log formatter.
#
# ```
# require "./custom_log_formatter"
# logger = Logger.new(STDOUT, Logger::DEBUG, custom_log_formatter)
# DEBUG [12:45:52.520 #13543] Hello world!
# ```
def custom_log_formatter
  Logger::Formatter.new do |severity, time, progname, message, io|
    fore, back = case severity
                 when Logger::DEBUG then {:dark_gray, nil}
                 when Logger::INFO  then {:green, nil}
                 when Logger::WARN  then {:yellow, nil}
                 when Logger::ERROR then {nil, :red}
                 when Logger::FATAL then {nil, :red}
                 else                    {nil, nil}
                 end

    prefix = (severity.to_s.rjust(5) + " [#{time.to_s("%X.%L")} ##{Process.pid}]").colorize
    prefix.fore(fore) if fore
    prefix.back(back) if back

    io << prefix << ' ' << message
  end
end
