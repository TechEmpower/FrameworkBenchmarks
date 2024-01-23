require "file_utils"

backend = Log::IOBackend.new
backend.formatter = Dexter::JSONLogFormatter.proc
Log.dexter.configure(:none, backend)

# Lucky only logs when before/after pipes halt by redirecting, or rendering a
# response. Pipes that run without halting are not logged.
#
# If you want to log every pipe that runs, set the log level to ':info'
Lucky::ContinuedPipeLog.dexter.configure(:none)

# Lucky only logs failed queries by default.
#
# Set the log to ':info' to log all queries
Avram::QueryLog.dexter.configure(:none)
