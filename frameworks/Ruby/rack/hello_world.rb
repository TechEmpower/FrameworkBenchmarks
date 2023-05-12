# frozen_string_literal: true

# Our Rack application to be executed by rackup
require "oj"
require_relative 'pg_db'

class HelloWorld
  QUERY_RANGE = 1..10_000 # range of IDs in the Fortune DB
  ALL_IDS = QUERY_RANGE.to_a # enumeration of all the IDs in fortune DB
  MIN_QUERIES = 1 # min number of records that can be retrieved
  MAX_QUERIES = 500 # max number of records that can be retrieved
  CONTENT_TYPE = "Content-Type"
  CONTENT_LENGTH = "Content-Length"
  JSON_TYPE = "application/json"
  HTML_TYPE = "text/html; charset=utf-8"
  PLAINTEXT_TYPE = "text/plain"
  DATE = "Date"
  SERVER = "Server"
  SERVER_STRING = if defined?(PhusionPassenger)
    "Passenger"
  elsif defined?(Puma)
    Puma::Const::PUMA_SERVER_STRING
  elsif defined?(Unicorn)
   "Unicorn"
  elsif defined?(Falcon)
    "Falcon"
  else
    " Ruby Rack"
  end
  TEMPLATE_PREFIX='<!DOCTYPE html>
<html>
<head>
  <title>Fortune</title>
</head>
<body>
  <table>
    <tr>
      <th>id</th>
      <th>message</th>
    </tr>'
    TEMPLATE_POSTFIX='</table>
    </body
  </html>'


def initialize
  @db=PgDb.new()
end

  def respond(content_type, body = "")
    [
      200,
      {
        CONTENT_TYPE => content_type,
        DATE => Time.now.utc.httpdate,
        SERVER => SERVER_STRING,
       CONTENT_LENGTH => body.length.to_s

      },
      [body]
    ]
  end
  def fortunes
    fortunes = @db.get_fortunes
    fortunes << {id: 0 , message: "Additional fortune added at request time."}
    fortunes.sort_by!{|item| item[:message]}
    buffer = String.new
     buffer << TEMPLATE_PREFIX

    fortunes.each do |item|
      buffer <<  "<tr><td> #{item[:id]} </td> <td>#{Rack::Utils.escape_html(item[:message])}</td></tr>"
    end
    buffer << TEMPLATE_POSTFIX
  end

  def call(env)
    case env["PATH_INFO"]
    when "/json"
      # Test type 1: JSON serialization
      respond JSON_TYPE,
              Oj.dump({ message: "Hello, World!" }, { mode: :strict })
    when "/db"
      # Test type 2: Single database query
      respond JSON_TYPE, Oj.dump(@db.get_random_record(), { mode: :strict })
    when "/queries"
      # Test type 3: Multiple database queries
       params = Rack::Utils.parse_query(env["QUERY_STRING"])
       queries = params["queries"]
       respond JSON_TYPE, Oj.dump(@db.get_multiple_records_async(queries), { mode: :strict } )
    when "/fortunes"
      # Test type 4: Fortunes
      respond HTML_TYPE, fortunes
    when "/updates"
      # Test type 5: Database updates
      params = Rack::Utils.parse_query(env["QUERY_STRING"])
      queries = params["queries"]
      respond JSON_TYPE, Oj.dump(@db.update_multiple_records_async(queries), { mode: :strict })
    when "/plaintext"
      # Test type 6: Plaintext
      respond PLAINTEXT_TYPE, "Hello, World!"
    end
  end

end
