require_relative 'hello_world'
use Rack::ContentLength
use Rack::Chunked
run HelloWorld.new
