# Enable Puma's clustered mode with about one process per 128 MiB of available
# memory, scaling up to as close to MAX_THREADS as possible. If there are fewer
# processes than MAX_THREADS, add threads per process to reach MAX_THREADS.
RESERVE_KB = 262_144
KB_PER_WORKER = 131_072 # average of peak PSS of puma processes (watch smem -U testrunner -k)
MIN_WORKERS = 2
MIN_THREADS_PER_WORKER = 4
MAX_THREADS = (ENV['MAX_CONCURRENCY'] || 256).to_i

def meminfo(arg)
  File.open('/proc/meminfo') do |f|
    f.each_line do |line|
      key, value = line.split(/:\s+/)
      return value.split(/\s+/).first.to_i if key == arg
    end
  end
end

# FWBM only... use puma_auto_tune in production!
avail_mem = meminfo('MemAvailable') - RESERVE_KB
num_workers = [[avail_mem / KB_PER_WORKER, MIN_WORKERS].max, MAX_THREADS].min
num_threads = num_workers < MAX_THREADS ? (1.0 * MAX_THREADS / num_workers).ceil : 0
num_threads = [num_threads, MIN_THREADS_PER_WORKER].max

workers num_workers
threads num_threads, num_threads
