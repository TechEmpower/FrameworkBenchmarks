Bundler.require('mysql')
opts = {
  adapter:  'mysql2',
  username: 'benchmarkdbuser',
  password: 'benchmarkdbpass',
  host:     'tfb-database',
  database: 'hello_world'
}

# Determine threading/thread pool size and timeout
if defined?(Puma) && (threads = Puma.cli_config.options.fetch(:max_threads)) > 1
  opts[:pool] = threads
  opts[:checkout_timeout] = 10
else
  # TODO: ActiveRecord doesn't have a single-threaded mode?
  opts[:pool] = 1
  opts[:checkout_timeout] = 0
end


# Setup our logger
ActiveRecord::Base.logger = logger

# Use ISO 8601 format for JSON serialized times and dates.
ActiveSupport.use_standard_json_time_format = true

# Don't escape HTML entities in JSON, leave that for the #json_escape helper
# if you're including raw JSON in an HTML page.
ActiveSupport.escape_html_entities_in_json = false

# Now we can establish connection with our db.
ActiveRecord::Base.establish_connection(opts)
