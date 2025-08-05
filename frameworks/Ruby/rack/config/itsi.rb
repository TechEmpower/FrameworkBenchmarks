require_relative 'auto_tune'

rackup_file './config.ru'

ruby_thread_request_backlog_size 10_000

preload false

num_workers, num_threads = auto_tune

workers num_workers
threads num_threads
fiber_scheduler false
