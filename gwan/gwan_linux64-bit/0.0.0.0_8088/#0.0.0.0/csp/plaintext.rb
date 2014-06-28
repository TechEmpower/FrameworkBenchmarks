#!/usr/bin/ruby
# Test type 6: Plaintext

require_relative 'gwan-helper'

content = "Hello, World!"

respond content, 'text/plain'