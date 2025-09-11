# frozen_string_literal: true

require_relative 'auto_tune'
require 'etc'
require 'sequel'

# FWBM only... use the puma_auto_tune gem in production!
num_workers, num_threads = auto_tune
num_threads = [num_threads, 32].min

before_fork do
  Sequel::DATABASES.each(&:disconnect)
end

if RUBY_PLATFORM == 'java'
  num_threads = 512
  num_workers = 0
end

workers num_workers
threads num_threads, num_threads
