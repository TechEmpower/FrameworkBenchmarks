# frozen_string_literal: true
require 'time'

# Our Rack application to be executed by rackup
class HelloWorld
  MAX_PK = 10_000
  ID_RANGE = (1..10_000).freeze
  ALL_IDS = ID_RANGE.to_a
  QUERIES_MIN = 1
  QUERIES_MAX = 500
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
                    'Puma'
                  elsif defined?(Iodine)
                    'Iodine'
                  elsif defined?(Unicorn)
                    'Unicorn'
                  else
                    'Ruby Rack'
                  end

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
    World::BY_ID.(id: rand1)
  end

  def queries(env)
    ids = ALL_IDS.sample(bounded_queries(env))
    DB.synchronize do
      ids.map do |id|
        World::BY_ID.(id: id)
      end
    end
  end

  def fortunes
    fortunes = Fortune.all
    fortunes << Fortune.new(
      id: 0,
      message: 'Additional fortune added at request time.'
    )
    fortunes.sort_by!(&:message)

    html = String.new(<<~'HTML')
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

    fortunes.each do |fortune|
      html << <<~"HTML"
      <tr>
        <td>#{fortune.id}</td>
        <td>#{Rack::Utils.escape_html(fortune.message)}</td>
      </tr>
      HTML
    end

    html << <<~'HTML'
      </table>

      </body>
      </html>
    HTML
  end

  def updates(env)
    ids = ALL_IDS.sample(bounded_queries(env))
    DB.synchronize do
      worlds =
        ids.map do |id|
          world = World::BY_ID.(id: id)
          world[:randomnumber] = rand1
          world
        end
      World.batch_update(worlds)
      worlds
    end
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
      headers(content_type, body),
      [body]
    ]
  end

  if defined?(Unicorn)
    def headers(content_type, body)
      {
        CONTENT_TYPE => content_type,
        SERVER => SERVER_STRING,
        CONTENT_LENGTH => body.bytesize.to_s
      }
    end
  elsif defined?(Puma)
    def headers(content_type, _)
      {
        CONTENT_TYPE => content_type,
        SERVER => SERVER_STRING,
        DATE => Time.now.utc.httpdate
      }
    end
  else
    def headers(content_type, _)
      {
        CONTENT_TYPE => content_type,
        SERVER => SERVER_STRING
      }
    end
  end
end
