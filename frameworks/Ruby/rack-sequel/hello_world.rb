# frozen_string_literal: true
require 'bundler/setup'
Bundler.require(:default) # Load core modules

require_relative 'db'
require 'time'

# Our Rack application to be executed by rackup
class HelloWorld
  MAX_PK = 10_000
  ID_RANGE = (1..10_000).freeze
  ALL_IDS = ID_RANGE.to_a
  QUERIES_MIN = 1
  QUERIES_MAX = 500

  CONTENT_TYPE = 'Content-Type'
  JSON_TYPE = 'application/json'
  HTML_TYPE = 'text/html; charset=utf-8'
  PLAINTEXT_TYPE = 'text/plain'
  DATE = 'Date'
  SERVER = 'Server'
  SERVER_STRING = 'Rack'

  TEMPLATE_PREFIX = <<~HTML
    <!DOCTYPE html>
    <html>
    <head>
      <title>Fortunes</title>
    </head>
    <body>
      <table>
        <tr>
          <th>id</th>
          <th>message</th>
        </tr>
  HTML

  TEMPLATE_POSTFIX = <<~HTML
      </table>
    </body>
    </html>
  HTML

  def bounded_queries(env)
    params = Rack::Utils.parse_query(env['QUERY_STRING'])

    queries = params['queries'].to_i
    queries.clamp(QUERIES_MIN, QUERIES_MAX)
  end

  # Return a random number between 1 and MAX_PK
  def rand1
    rand(MAX_PK).succ
  end

  def db
    World.with_pk(rand1).values
  end

  def queries(env)
    ids = ALL_IDS.sample(bounded_queries(env))
    DB.synchronize do
      ids.map do |id|
        World.with_pk(id).values
      end
    end
  end

  def fortunes
    fortunes = Fortune.all

    fortune = Fortune.new
    fortune.id = 0
    fortune.message = "Additional fortune added at request time."
    fortunes << fortune

    fortunes.sort_by!(&:message)

    buffer = String.new
    buffer << TEMPLATE_PREFIX
    fortunes.each do |item|
      buffer << "<tr><td>#{item.id}</td><td>#{ERB::Escape.html_escape(item.message)}</td></tr>"
    end
    buffer << TEMPLATE_POSTFIX
  end

  def updates(env)
    worlds = []
    ids = ALL_IDS.sample(bounded_queries(env))
    DB.synchronize do
      worlds =
        ids.map do |id|
          world = World.with_pk(id)
          new_value = rand1
          new_value = rand1 while new_value == world.randomnumber
          world.randomnumber = new_value
          world
        end
      World.batch_update(worlds)
    end
    worlds.map!(&:values)
  end

  def call(env)
    case env['PATH_INFO']
    when '/json'
      # Test type 1: JSON serialization
      respond JSON_TYPE, { message: 'Hello, World!' }.to_json
    when '/db'
      # Test type 2: Single database query
      respond JSON_TYPE, db.to_json
    when '/queries'
      # Test type 3: Multiple database queries
      respond JSON_TYPE, queries(env).to_json
    when '/fortunes'
      # Test type 4: Fortunes
      respond HTML_TYPE, fortunes
    when '/updates'
      # Test type 5: Database updates
      respond JSON_TYPE, updates(env).to_json
    when '/plaintext'
      # Test type 6: Plaintext
      respond PLAINTEXT_TYPE, 'Hello, World!'
    end
  end

  private

  def respond(content_type, body)
    [
      200,
      headers(content_type),
      [body]
    ]
  end

  if defined?(Puma)
    def headers(content_type)
      {
        CONTENT_TYPE => content_type,
        SERVER => SERVER_STRING,
        DATE => Time.now.httpdate
      }
    end
  else
    def headers(content_type)
      {
        CONTENT_TYPE => content_type,
        SERVER => SERVER_STRING
      }
    end
  end
end
