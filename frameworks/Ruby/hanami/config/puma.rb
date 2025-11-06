# frozen_string_literal: true
threads 3, 3

port        ENV.fetch("HANAMI_PORT", 2300)
environment ENV.fetch("HANAMI_ENV", "development")

on_worker_boot do
  Hanami.shutdown
  Hanami.boot
end

preload_app!
