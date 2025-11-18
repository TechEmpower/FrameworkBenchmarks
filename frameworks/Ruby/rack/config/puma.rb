require_relative 'auto_tune'

# FWBM only... use the puma_auto_tune gem in production!
_, num_threads = auto_tune

if RUBY_PLATFORM == 'java'
  num_threads = 512
  num_workers = 0
end

threads num_threads

before_fork do
  Sequel::DATABASES.each(&:disconnect)
end
