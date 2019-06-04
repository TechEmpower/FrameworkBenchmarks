# frozen_string_literal: true

require 'agoo'
require 'connection_pool'
require 'oj'
require 'pg'
require 'rack'

$pool = ConnectionPool.new(size: 256, timeout: 5) do
          PG::Connection.new({
                                 dbname:   'hello_world',
                                 host:     'tfb-database',
                                 user:     'benchmarkdbuser',
                                 password: 'benchmarkdbpass'
                             })
        end

class BaseHandler
  def self.extract_queries_param(request = nil)
    queries = Rack::Utils.parse_query(request['QUERY_STRING'])['queries'].to_i rescue 1

    return   1 if queries <   1
    return 500 if queries > 500

    queries
  end

  def self.get_one_random_number
    1 + Random.rand(10000)
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
            'Content-Type' => 'text/html; charset=utf-8',
            'Date'         => Time.now.utc.httpdate,
            'Server'       => 'Agoo'
        },
        [str]
    ]
  end

  def self.json_response(obj = {})
    [
        200,
        {
            'Content-Type' => 'application/json',
            'Date'         => Time.now.utc.httpdate,
            'Server'       => 'Agoo'
        },
        [Oj.dump(obj, { :mode => :strict })]
    ]
  end

  def self.plain_response(str = '')
    [
        200,
        {
            'Content-Type' => 'text/plain',
            'Date'         => Time.now.utc.httpdate,
            'Server'       => 'Agoo'
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
              sort { |x,y| x['message'] <=> y['message'] }.
                map { |f| "<tr><td>#{ f['id'] }</td><td>#{ Rack::Utils.escape_html(f['message']) }</td></tr>" }.
                  join

    html_response(<<-HTML)
      <!DOCTYPE html>
      <html>
        <head>
          <title>Fortune</title>
        </head>
        <body>
          <table>
            <tr>
              <th>id</th>
              <th>message</th>
            </tr>
            #{ f_2 }
          </table>
        </body
      </html>
    HTML
  end
end

class QueriesHandler < BaseHandler
  def self.call(req)
    records =
        [].tap do|r|
          (extract_queries_param req).times do
            r << get_one_record()
          end
        end

    json_response(records)
  end
end

class UpdatesHandler < BaseHandler
  def self.call(req)
    records =
        [].tap do|r|
          (extract_queries_param req).times do
            r << get_one_record()
          end
        end

    updated_records =
        records.map { |r| r['randomnumber'] = get_one_random_number; r }

    sql_values =
        updated_records.
          map { |r| "(#{ r['id'] }, #{ r['randomnumber'] })"}.
            join(', ')

    $pool.with do |conn|
      conn.exec(<<-SQL)

        UPDATE world AS ori
           SET randomnumber = new.randomnumber
          FROM (VALUES #{ sql_values }) AS new (id, randomnumber)
         WHERE ori.id = new.id

      SQL
    end

    json_response(updated_records)
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
