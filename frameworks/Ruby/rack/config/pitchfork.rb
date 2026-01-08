# frozen_string_literal: true

require 'etc'
require 'sequel'

worker_processes (Etc.nprocessors * 1.5).to_i

before_fork do |_server|
  Sequel::DATABASES.each(&:disconnect)
end

listen "/tmp/.sock", :backlog => 4096
