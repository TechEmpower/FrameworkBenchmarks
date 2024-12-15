# frozen_string_literal: true

require 'agoo'
require 'connection_pool'
require 'oj'
require 'pg'
require 'rack'

$pool = ConnectionPool.new(size: 1, timeout: 5) do
          PG::Connection.new({
                                 dbname:   'hello_world',
                                 host:     'tfb-database',
                                 user:     'benchmarkdbuser',
                                 password: 'benchmarkdbpass'
                             })
        end

QUERY_RANGE = (1..10_000).freeze
ALL_IDS = QUERY_RANGE.to_a
QUERIES_MIN = 1
QUERIES_MAX = 500

CONTENT_TYPE = 'Content-Type'
CONTENT_LENGTH = 'Content-Length'
DATE = 'Date'
SERVER = 'Server'
SERVER_STRING = 'Agoo'

JSON_TYPE = 'application/json'
HTML_TYPE = 'text/html; charset=utf-8'
PLAINTEXT_TYPE = 'text/plain'


class BaseHandler
  def self.extract_queries_param(request = nil)
    queries = Rack::Utils.parse_query(request['QUERY_STRING'])['queries'].to_i rescue 1

    queries.clamp(QUERIES_MIN, QUERIES_MAX)
  end

  def self.get_one_random_number
    Random.rand(QUERY_RANGE)
  end

  def self.get_one_record(id = get_one_random_number)
    $pool.with do |conn|
      conn.exec_params(<<-SQL, [id]).first

        SELECT * FROM world WHERE id = $1

      SQL
    end
  end

  def self.html_response(str = '')
    [
        200,
        {
            CONTENT_TYPE => HTML_TYPE,
            DATE         => Time.now.httpdate,
            SERVER       => SERVER_STRING
        },
        [str]
    ]
  end

  def self.json_response(obj = {})
    [
        200,
        {
            CONTENT_TYPE => JSON_TYPE,
            DATE         => Time.now.httpdate,
            SERVER       => SERVER_STRING
        },
        [Oj.dump(obj, { :mode => :strict })]
    ]
  end

  def self.plain_response(str = '')
    [
        200,
        {
            CONTENT_TYPE => PLAINTEXT_TYPE,
            DATE         => Time.now.httpdate,
            SERVER       => SERVER_STRING
        },
        [str]
    ]
  end
end

class PlaintextHandler < BaseHandler
  def self.call(_req)
    plain_response('Hello, World!')
  end

  def static?
    true
  end
end

class JsonHandler < BaseHandler
  def self.call(_req)
    json_response({ :message => "Hello, World!" })
  end

  def static?
    true
  end
end

class DbHandler < BaseHandler
  def self.call(_req)
    json_response(get_one_record)
  end
end

class FortunesHandler < BaseHandler
  def self.call(_req)
    f_1 = $pool.with do |conn|
            conn.exec(<<-SQL)

              SELECT id, message FROM fortune

            SQL
          end

    f_2 = f_1.map(&:to_h).
            append({ 'id' => '0', 'message' => 'Additional fortune added at request time.' }).
              sort_by { |item| item['message'] }.
                map { |f| "<tr><td>#{ f['id'] }</td><td>#{ Rack::Utils.escape_html(f['message']) }</td></tr>" }.
                  join

    html_response(<<-HTML)
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
            #{ f_2 }
          </table>
        </body>
      </html>
    HTML
  end
end

class QueriesHandler < BaseHandler
  def self.call(req)
    queries = extract_queries_param req
    records = ALL_IDS.sample(queries).map do |id|
      get_one_record(id)
    end

    json_response(records)
  end
end

class UpdatesHandler < BaseHandler
  def self.call(req)
    queries = extract_queries_param req
    records = ALL_IDS.sample(queries).sort.map do |id|
      world = get_one_record(id)
      world['randomnumber'] = get_one_random_number
      world
    end

    sql_values =
        records.
          map { |r|
            "(#{ r['id'] }, #{ r['randomnumber'] })"
          }.join(', ')

    $pool.with do |conn|
      conn.exec(<<-SQL)

        UPDATE world AS ori
           SET randomnumber = new.randomnumber
          FROM (VALUES #{ sql_values }) AS new (id, randomnumber)
         WHERE ori.id = new.id

      SQL
    end

    json_response(records)
  end
end

Agoo::Log.configure({
                        classic:  true,
                        colorize: true,
                        console:  true,
                        dir:      '',
                        states:   {
                            DEBUG:    false,
                            INFO:     false,

                            connect:  false,
                            eval:     false,
                            push:     false,
                            request:  false,
                            response: false
                        }
                    })

worker_count = 4
worker_count = ENV['AGOO_WORKER_COUNT'].to_i if ENV.key?('AGOO_WORKER_COUNT')

Agoo::Server.init(8080, '.', thread_count: 0, worker_count: worker_count)

Agoo::Server.handle(:GET, '/plaintext', PlaintextHandler)
Agoo::Server.handle(:GET, '/json',      JsonHandler)
Agoo::Server.handle(:GET, '/db',        DbHandler)
Agoo::Server.handle(:GET, '/fortunes',  FortunesHandler)
Agoo::Server.handle(:GET, '/queries',   QueriesHandler)
Agoo::Server.handle(:GET, '/updates',   UpdatesHandler)

Agoo::Server.start
