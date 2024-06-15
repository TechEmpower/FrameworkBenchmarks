# frozen_string_literal: true

# Our Rack application to be executed by rackup

require_relative 'pg_db'
require_relative 'config/auto_tune'
require 'rack'

if RUBY_PLATFORM == 'java'
  require 'json'
  DEFAULT_DATABASE_URL = 'jdbc:postgresql://tfb-database/hello_world?user=benchmarkdbuser&password=benchmarkdbpass'
else
  require 'oj'
  Oj.mimic_JSON
  DEFAULT_DATABASE_URL = 'postgresql://tfb-database/hello_world?user=benchmarkdbuser&password=benchmarkdbpass'
end

class HelloWorld
  QUERY_RANGE = (1..10_000).freeze # range of IDs in the Fortune DB
  ALL_IDS = QUERY_RANGE.to_a # enumeration of all the IDs in fortune DB
  MIN_QUERIES = 1 # min number of records that can be retrieved
  MAX_QUERIES = 500 # max number of records that can be retrieved
  CONTENT_TYPE = 'Content-Type'
  CONTENT_LENGTH = 'Content-Length'
  JSON_TYPE = 'application/json'
  HTML_TYPE = 'text/html; charset=utf-8'
  PLAINTEXT_TYPE = 'text/plain'
  DATE = 'Date'
  SERVER = 'Server'
  SERVER_STRING = if defined?(PhusionPassenger)
                    'Passenger'
                  elsif defined?(Puma)
                    Puma::Const::PUMA_SERVER_STRING
                  elsif defined?(Unicorn)
                    'Unicorn'
                  elsif defined?(Falcon)
                    'Falcon'
                  else
                    ' Ruby Rack'
                  end
  TEMPLATE_PREFIX = '<!DOCTYPE html>
<html>
<head>
  <title>Fortunes</title>
</head>
<body>
  <table>
    <tr>
      <th>id</th>
      <th>message</th>
    </tr>'
  TEMPLATE_POSTFIX = '</table>
    </body>
  </html>'

  def initialize
    if defined?(Puma)
      num_workers, num_threads = auto_tune
      num_threads = [num_threads, 32].min
      max_connections = num_workers * num_threads
    else
      max_connections = 512
    end
    @db = PgDb.new(DEFAULT_DATABASE_URL, max_connections)
  end

  def respond(content_type, body = '')
    headers = {
      CONTENT_TYPE => content_type,
      DATE => Time.now.utc.httpdate,
      SERVER => SERVER_STRING
    }
    headers[CONTENT_LENGTH] = body.bytesize.to_s if defined?(Unicorn)
    [
      200,
      headers,
      [body]
    ]
  end

  def fortunes
    fortunes = @db.select_fortunes
    fortunes << { id: 0, message: 'Additional fortune added at request time.' }
    fortunes.sort_by! { |item| item[:message] }
    buffer = String.new
    buffer << TEMPLATE_PREFIX

    fortunes.each do |item|
      buffer << "<tr><td>#{item[:id]}</td><td>#{Rack::Utils.escape_html(item[:message])}</td></tr>"
    end
    buffer << TEMPLATE_POSTFIX
  end

  def call(env)
    case env['PATH_INFO']
    when '/json'
      # Test type 1: JSON serialization
      respond JSON_TYPE,
              { message: 'Hello, World!' }.to_json
    when '/db'
      # Test type 2: Single database query
      respond JSON_TYPE, @db.select_random_world.to_json
    when '/queries'
      # Test type 3: Multiple database queries
      params = Rack::Utils.parse_query(env['QUERY_STRING'])
      queries = params['queries']
      respond JSON_TYPE, @db.select_worlds(queries).to_json
    when '/fortunes'
      # Test type 4: Fortunes
      respond HTML_TYPE, fortunes
    when '/updates'
      # Test type 5: Database updates
      params = Rack::Utils.parse_query(env['QUERY_STRING'])
      queries = params['queries']
      respond JSON_TYPE, @db.update_worlds(queries).to_json
    when '/plaintext'
      # Test type 6: Plaintext
      respond PLAINTEXT_TYPE, 'Hello, World!'
    end
  end
end
