require_relative 'auto_tune'

# FWBM only... use the puma_auto_tune gem in production!
num_workers, num_threads = auto_tune

workers num_workers
threads num_threads, num_threads
