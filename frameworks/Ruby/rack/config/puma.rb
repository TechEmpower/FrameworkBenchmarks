# frozen_string_literal: true

require_relative 'auto_tune'
require 'etc'
require 'sequel'

# FWBM only... use the puma_auto_tune gem in production!
num_workers, num_threads = auto_tune

threads = Etc.nprocessors * 2

if RUBY_PLATFORM != 'java'
  workers num_workers
  threads num_threads, num_threads
end

before_fork do
  Sequel::DATABASES.each(&:disconnect)
end

# Use the `preload_app!` method when specifying a `workers` number.
# This directive tells Puma to first boot the application and load code
# before forking the application. This takes advantage of Copy On Write
# process behavior so workers use less memory.
#
preload_app!
