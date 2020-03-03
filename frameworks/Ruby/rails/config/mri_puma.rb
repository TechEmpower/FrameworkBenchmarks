require_relative 'auto_tune'

# FWBM only... use the puma_auto_tune gem in production!
num_workers, num_threads = auto_tune

workers num_workers
threads num_threads, num_threads

preload_app!

# If you are preloading your application and using Active Record, it's
# recommended that you close any connections to the database before workers
# are forked to prevent connection leakage.
#
before_fork do
  ActiveRecord::Base.connection_pool.disconnect! if defined?(ActiveRecord)
end

# The code in the `on_worker_boot` will be called if you are using
# clustered mode by specifying a number of `workers`. After each worker
# process is booted, this block will be run. If you are using the `preload_app!`
# option, you will want to use this block to reconnect to any threads
# or connections that may have been created at application boot, as Ruby
# cannot share connections between processes.
#
on_worker_boot do
  ActiveRecord::Base.establish_connection if defined?(ActiveRecord)
end
