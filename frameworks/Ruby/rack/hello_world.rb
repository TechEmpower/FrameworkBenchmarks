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
  JSON_TYPE = "application/json"
  HTML_TYPE = "text/html; charset=utf-8"
  PLAINTEXT_TYPE = "text/plain"
  #SERVER_STRING = "Ruby Rack"
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
def test_database
  pp @db.get_one_record
  pp @db.update_one_record
  pp @db.get_fortunes
end
  def respond(content_type, body = "")
    [
      200,
      {
        CONTENT_TYPE => content_type,
        DATE => Time.now.utc.httpdate,
        SERVER => SERVER_STRING
        #'Content-Length' => body.length.to_s,
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
      ["application/json", JSON.fast_generate(db)]
    when "/queries"
      # Test type 3: Multiple database queries
      ["application/json", JSON.fast_generate(queries(env))]
    when "/fortunes"
      # Test type 4: Fortunes
      respond HTML_TYPE, fortunes
    when "/updates"
      # Test type 5: Database updates
      ["application/json", JSON.fast_generate(updates(env))]
    when "/plaintext"
      # Test type 6: Plaintext
      respond PLAINTEXT_TYPE, "Hello, World!"
    end
  end

end


 #   fortunes << Fortune.new(
    #     id: 0,
    #     message: "Additional fortune added at request time."
    #   )
    #   fortunes.sort_by!(&:message)
    #   html = String.new(<<~'HTML')
    #     <!DOCTYPE html>
    #     <html>
    #     <head>
    #       <title>Fortunes</title>
    #     </head>
    #     <body>
    #     <table>
    #     <tr>
    #       <th>id</th>
    #       <th>message</th>
    #     </tr>
    #   HTML
    #   fortunes.each { |fortune| html << <<~"HTML" }
    #     <tr>
    #       <td>#{fortune.id}</td>
    #       <td>#{Rack::Utils.escape_html(fortune.message)}</td>
    #     </tr>
    #     HTML
    #   html << <<~'HTML'
    #     </table>
    #     </body>
    #     </html>
    #   HTML
    # end
    # def updates(env)
    #   DB.synchronize do
    #     Array.new(bounded_queries(env)) do
    #       world = WORLD_BY_ID.(id: rand1)
    #       WORLD_UPDATE.(
    #         id: world[:id],
    #         randomnumber: (world[:randomnumber] = rand1)
    #       )
    #       world
    #     end
    #   end
    # end

  #   def html_response(str = "")
  #     [
  #       200,
  #       {
  #         CONTENT_TYPE => HTML_TYPE,
  #         DATE => Time.now.utc.httpdate,
  #         SERVER => SERVER_STRING
  #       },
  #       str
  #     ]
  #   end

  #   def self.json_response(obj = {})
  #     [
  #       200,
  #       {
  #         CONTENT_TYPE => JSON_TYPE,
  #         DATE => Time.now.utc.httpdate,
  #         SERVER => SERVER_STRING
  #       },
  #       [Oj.dump(obj, { mode: :strict })]
  #     ]
  #   end

  #   def self.plain_response(str = "")
  #     [
  #       200,
  #       {
  #         CONTENT_TYPE => PLAINTEXT_TYPE,
  #         DATE => Time.now.utc.httpdate,
  #         SERVER => SERVER_STRING
  #       },
  #       str
  #     ]
  #   end
  # end

  # def bounded_queries(env)
  #   params = Rack::Utils.parse_query(env["QUERY_STRING"])
  #   queries = params["queries"].to_i
  #   return QUERIES_MIN if queries < QUERIES_MIN
  #   return QUERIES_MAX if queries > QUERIES_MAX
  #   queries
  # end
  # # Return a random number between 1 and MAX_PK
  # def rand1
  #   rand(MAX_PK).succ
  # end
  # WORLD_BY_ID = World.naked.where(id: :$id).prepare(:first, :world_by_id)
  # WORLD_UPDATE =
  #   World.where(id: :$id).prepare(
  #     :update,
  #     :world_update,
  #     randomnumber: :$randomnumber
  #   )
  # def db
  #   WORLD_BY_ID.(id: rand1)
  # end
  # def queries(env)
  #   DB.synchronize do
  #     Array.new(bounded_queries(env)) { WORLD_BY_ID.(id: rand1) }
  #   end
  # end
  # def fortunes
  #   fortunes = Fortune.all
  #   fortunes << Fortune.new(
  #     id: 0,
  #     message: "Additional fortune added at request time."
  #   )
  #   fortunes.sort_by!(&:message)
  #   html = String.new(<<~'HTML')
  #     <!DOCTYPE html>
  #     <html>
  #     <head>
  #       <title>Fortunes</title>
  #     </head>
  #     <body>
  #     <table>
  #     <tr>
  #       <th>id</th>
  #       <th>message</th>
  #     </tr>
  #   HTML
  #   fortunes.each { |fortune| html << <<~"HTML" }
  #     <tr>
  #       <td>#{fortune.id}</td>
  #       <td>#{Rack::Utils.escape_html(fortune.message)}</td>
  #     </tr>
  #     HTML
  #   html << <<~'HTML'
  #     </table>
  #     </body>
  #     </html>
  #   HTML
  # end
  # def updates(env)
  #   DB.synchronize do
  #     Array.new(bounded_queries(env)) do
  #       world = WORLD_BY_ID.(id: rand1)
  #       WORLD_UPDATE.(
  #         id: world[:id],
  #         randomnumber: (world[:randomnumber] = rand1)
  #       )
  #       world
  #     end
  #   end
  # end
