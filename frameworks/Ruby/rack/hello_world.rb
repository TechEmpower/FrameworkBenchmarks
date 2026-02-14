# frozen_string_literal: true

# Our Rack application to be executed by rackup

if RUBY_PLATFORM == 'java'
  require_relative 'db_jruby'
else
  require_relative 'db'
end
require_relative 'config/auto_tune'
require 'rack'
require 'json'
require 'time'
require 'erb'

DATABASE_URL = 'postgresql://tfb-database/hello_world?user=benchmarkdbuser&password=benchmarkdbpass'
$db = connect(DATABASE_URL)

class HelloWorld
  QUERY_RANGE = (1..10_000).freeze # range of IDs in the Fortune DB
  ALL_IDS = QUERY_RANGE.to_a # enumeration of all the IDs in fortune DB
  MIN_QUERIES = 1 # min number of records that can be retrieved
  MAX_QUERIES = 500 # max number of records that can be retrieved

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

  def call(env)
    case env['PATH_INFO']
    when '/json'
      # Test type 1: JSON serialization
      respond JSON_TYPE,
              { message: 'Hello, World!' }.to_json
    when '/db'
      # Test type 2: Single database query
      id = random_id
      respond JSON_TYPE, $db.with{ _1.select_world(id) }.to_json
    when '/queries'
      # Test type 3: Multiple database queries
      queries = bounded_queries(env)
      respond JSON_TYPE, select_worlds(queries).to_json
    when '/fortunes'
      # Test type 4: Fortunes
      respond HTML_TYPE, fortunes
    when '/updates'
      # Test type 5: Database updates
      queries = bounded_queries(env)
      respond JSON_TYPE, update_worlds(queries).to_json
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

  if defined?(Puma) || defined?(Falcon)
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

  def fortunes
    fortunes = $db.with(&:select_fortunes).map(&:to_h)
    fortunes << { 'id' => 0, 'message' => 'Additional fortune added at request time.' }
    fortunes.sort_by! { |item| item['message'] }

    buffer = String.new
    buffer << TEMPLATE_PREFIX
    fortunes.each do |item|
      buffer << "<tr><td>#{item['id']}</td><td>#{ERB::Escape.html_escape(item['message'])}</td></tr>"
    end
    buffer << TEMPLATE_POSTFIX
  end

  def update_worlds(count)
    results = select_worlds(count)
    ids = []
    sql = String.new("UPDATE world SET randomnumber = CASE id ")
    results.each do |r|
      r['randomnumber'] = random_id
      ids << r['id']
      sql << "when #{r['id']} then #{r['randomnumber']} "
    end
    sql << "ELSE randomnumber END WHERE id IN ( #{ids.join(',')})"
    $db.with{ _1.exec(sql) }
    results
  end

  def select_worlds(count)
    ids = ALL_IDS.sample(count)
    $db.with do |conn|
      ids.map do |id|
        conn.select_world(id)
      end
    end
  end

  def random_id
    Random.rand(QUERY_RANGE)
  end

  def bounded_queries(env)
    params = Rack::Utils.parse_query(env['QUERY_STRING'])

    queries = params['queries'].to_i
    queries.clamp(MIN_QUERIES, MAX_QUERIES)
  end
end
