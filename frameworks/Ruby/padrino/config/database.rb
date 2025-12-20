Bundler.require('trilogy')
opts = {
  adapter:  'trilogy',
  username: 'benchmarkdbuser',
  password: 'benchmarkdbpass',
  host:     'tfb-database',
  database: 'hello_world',
  ssl:      true,
  ssl_mode: 4, # Trilogy::SSL_PREFERRED_NOVERIFY
  tls_min_version: 3 # Trilogy::TLS_VERSION_12
}

# Determine threading/thread pool size and timeout
# TODO: ActiveRecord doesn't have a single-threaded mode?
opts[:pool] = 512
opts[:checkout_timeout] = 5

# Setup our logger
ActiveRecord::Base.logger = logger

# Use ISO 8601 format for JSON serialized times and dates.
ActiveSupport.use_standard_json_time_format = true

# Don't escape HTML entities in JSON, leave that for the #json_escape helper
# if you're including raw JSON in an HTML page.
ActiveSupport.escape_html_entities_in_json = false

# Now we can establish connection with our db.
ActiveRecord::Base.establish_connection(opts)
