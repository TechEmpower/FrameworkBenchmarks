#!/usr/bin/env ruby
# frozen_string_literal: true

# Instantiate about one process per X MiB of available memory, scaling up to as
# close to MAX_THREADS as possible while observing an upper bound based on the
# number of virtual/logical CPUs. If there are fewer processes than
# MAX_THREADS, add threads per process to reach MAX_THREADS.
require 'etc'

KB_PER_WORKER = 128 * 1_024 # average of peak PSS of single-threaded processes (watch smem -k)
MIN_WORKERS = 15
MAX_WORKERS_PER_VCPU = 1.25 # virtual/logical
MIN_THREADS_PER_WORKER = 1
MAX_THREADS = Integer(ENV['MAX_CONCURRENCY'] || 256)

def meminfo(arg)
  File.open('/proc/meminfo') do |f|
    f.each_line do |line|
      key, value = line.split(/:\s+/)
      return value.split(/\s+/).first.to_i if key == arg
    end
  end

  raise "Unable to find `#{arg}' in /proc/meminfo!"
end

def auto_tune
  avail_mem = meminfo('MemAvailable') * 0.8 - MAX_THREADS * 1_024

  workers = [
    [(1.0 * avail_mem / KB_PER_WORKER).floor, MIN_WORKERS].max,
    [(Etc.nprocessors * MAX_WORKERS_PER_VCPU).ceil, MIN_WORKERS].max
  ].min

  threads_per_worker = [
    workers < MAX_THREADS ? (1.0 * MAX_THREADS / workers).ceil : -Float::INFINITY,
    MIN_THREADS_PER_WORKER
  ].max

  [workers, threads_per_worker]
end

p auto_tune if $PROGRAM_NAME == __FILE__
