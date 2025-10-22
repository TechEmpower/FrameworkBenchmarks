# frozen_string_literal: true

require_relative 'auto_tune'
require 'sequel'
num_workers, = auto_tune

worker_processes num_workers

before_fork do |_server|
  Sequel::DATABASES.each(&:disconnect)
end
