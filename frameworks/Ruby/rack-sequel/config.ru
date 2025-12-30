require_relative 'boot'
require_relative 'hello_world'
use Rack::ContentLength
run HelloWorld.new
