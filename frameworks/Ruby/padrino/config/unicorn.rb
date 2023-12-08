require_relative 'auto_tune'

num_workers, = auto_tune

worker_processes num_workers
listen "/tmp/.sock", :backlog => 4096

preload_app true
GC.respond_to?(:copy_on_write_friendly=) and GC.copy_on_write_friendly = true

before_fork do |server, worker|
end

after_fork do |server, worker|
end
