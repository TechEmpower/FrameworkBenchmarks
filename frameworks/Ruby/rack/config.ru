#Bundler.require :default
require_relative "hello_world.rb"

if defined?(Falcon)
  puts "Falcon defined"
else
  puts "not falcon"
end

run HelloWorld.new
#require "erb"
#require "yaml"

#$: << "."

#DB_CONFIG = YAML.load(ERB.new(File.read("config/database.yml")).result)

#if RUBY_PLATFORM == "java"
#  require "app/jruby_impl"
#  run App::JRuby
#else
#  require "app/ruby_impl"
#  run App::Ruby
#end
