# frozen_string_literal: true
require_relative 'hello_world'

use Rack::ContentLength
run HelloWorld.new
