#!/usr/bin/ruby
# Test type 1: JSON serialization

require 'json'
require_relative 'gwan-helper'

json = {message: 'Hello, World!'}
content = json.to_json

respond content, 'application/json'