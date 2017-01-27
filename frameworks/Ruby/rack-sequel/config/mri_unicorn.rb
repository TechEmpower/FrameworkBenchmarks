require_relative 'auto_tune'

# FWBM only...
num_workers, = auto_tune

worker_processes num_workers
