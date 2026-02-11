# frozen_string_literal: true
require 'etc'

worker_processes (Etc.nprocessors * 1.5).to_i

listen "/tmp/.sock", :backlog => 4096
