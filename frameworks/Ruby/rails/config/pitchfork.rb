# frozen_string_literal: true

require_relative 'auto_tune'

num_workers, = auto_tune

worker_processes num_workers

listen "/tmp/.sock", :backlog => 4096
