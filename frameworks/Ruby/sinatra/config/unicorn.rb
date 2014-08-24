worker_processes 8
listen "/tmp/.sock", :backlog => 256
preload_app true
GC.respond_to?(:copy_on_write_friendly=) and GC.copy_on_write_friendly = true

before_fork { |server, worker| }
after_fork { |server, worker| }