#!/usr/bin/ruby
require 'time'

HTTP_HEADER = "HTTP/1.1 200 OK
Content-Type: %s; charset=UTF-8
Content-Length: %d
Server: G-WAN
Date: %s

"
HTML_ENTITIES = {
    "'"  => "&#39;",
    "&"  => "&amp;",
    "\"" => "&quot;",
    "<"  => "&lt;",
    ">"  => "&gt;"
}

def respond content, content_type = 'text/html'
  puts HTTP_HEADER % [ content_type, content.length, Time.now.httpdate ]
  puts content
  exit 1
end

def request
  res = {}
  ARGV.each do |arg|
    arg = arg.split('=')
    res[arg.first.to_sym] = arg.last
  end
  return res
end

def escape str
  HTML_ENTITIES.each { |k,v| str.gsub! k,v }
  return str
end