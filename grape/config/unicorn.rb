worker_processes 8
listen 8080

preload_app true
GC.respond_to?(:copy_on_write_friendly=) and GC.copy_on_write_friendly = true

before_fork do |server, worker|
end

after_fork do |server, worker|
end
