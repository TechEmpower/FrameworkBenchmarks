Bundler.require :default
require 'erb'

$: << "."

DB_CONFIG = YAML.load(ERB.new(File.read("config/database.yml")).result)

if RUBY_PLATFORM == 'java'
 require 'app/jruby_impl'
 run App::JRuby
else
 require 'app/ruby_impl'
 run App::Ruby
end