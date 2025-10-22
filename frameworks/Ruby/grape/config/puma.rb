require_relative 'auto_tune'

# FWBM only... use the puma_auto_tune gem in production!
_, num_threads = auto_tune

threads num_threads

# Use the `preload_app!` method when specifying a `workers` number.
# This directive tells Puma to first boot the application and load code
# before forking the application. This takes advantage of Copy On Write
# process behavior so workers use less memory.
#
preload_app!
