if ENV.fetch('WEB_CONCURRENCY').to_i > 1
  before_fork do
    Sequel::DATABASES.each(&:disconnect)
  end
else
  workers ENV.fetch('WEB_CONCURRENCY')
  require 'concurrent/utility/processor_counter'
  threads = (::Concurrent.available_processor_count * 1.5).to_i
  threads threads
  ENV['MAX_THREADS'] = threads.to_s
end
