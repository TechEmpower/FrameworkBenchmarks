# frozen_string_literal: true

require 'json'
require 'yaml'

yaml = YAML.load(ARGF.read)
yaml["tests"][0].delete_if { |_, v| v["disabled"] }
puts JSON.pretty_generate(yaml)
