require_relative 'auto_tune'

num_workers, num_threads = auto_tune

workers num_workers
threads num_threads, num_threads

environment 'production'
bind 'tcp://0.0.0.0:8080'
