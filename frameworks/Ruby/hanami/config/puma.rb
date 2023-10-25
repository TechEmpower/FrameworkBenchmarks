# frozen_string_literal: true
require_relative 'auto_tune'

# FWBM only... use the puma_auto_tune gem in production!
num_workers, num_threads = auto_tune

workers num_workers
threads num_threads, num_threads

port        ENV.fetch("HANAMI_PORT", 2300)
environment ENV.fetch("HANAMI_ENV", "development")

on_worker_boot do
  Hanami.shutdown
  Hanami.boot
end

preload_app!
