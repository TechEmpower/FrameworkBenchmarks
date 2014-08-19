require 'java'
Jdbc::MySQL.load_driver
Java::com.mysql.jdbc.Driver

host, database     = DB_CONFIG[:host], DB_CONFIG[:database]
username, password = DB_CONFIG[:username], DB_CONFIG[:password]
JDBC_CONFIG = "jdbc:mysql://#{host}/#{database}?user=#{username}&password=#{password}"

module App
  JRuby = lambda do |env| 
    content_type, body = case env['PATH_INFO']
      when '/plaintext'
        ['text/plain; charset=utf-8', "Hello, World!"] 
      when '/json'
        ['application/json; charset=utf-8', {:message => "Hello, World!"}.to_json]
      when '/db'
        id = Random.rand(10000) + 1
        query = "SELECT * FROM World WHERE id = " + id.to_s

        connection = java.sql.DriverManager.get_connection(JDBC_CONFIG)
        results = begin
          statement = connection.create_statement
          begin
            rs = statement.execute_query(query)
            rs.next
            {id: rs.getObject('id'), randomNumber: rs.getObject('randomNumber')}
          ensure
            statement.close
          end
        ensure
          connection.close
        end
        ['application/json; charset=utf-8', results.to_json]
      when '/queries'
        query_string = Rack::Utils.parse_query(env['QUERY_STRING'])
        queries = query_string['queries'].to_i
        queries = 1 if queries < 1
        queries = 500 if queries > 500

        connection = java.sql.DriverManager.get_connection(JDBC_CONFIG)
        results = begin
          statement = connection.create_statement
          begin
            (1..queries).map do
              id = Random.rand(10000) + 1
              rs = statement.execute_query("SELECT * FROM World WHERE id = " + id.to_s)
              rs.next
              {id: rs.getObject('id'), randomNumber: rs.getObject('randomNumber')}
            end
          ensure
            statement.close
          end
        ensure
          connection.close
        end
        ['application/json; charset=utf-8', results.to_json] 
      when '/updates'
        query_string = Rack::Utils.parse_query(env['QUERY_STRING'])
        queries = query_string['queries'].to_i
        queries = 1 if queries < 1
        queries = 500 if queries > 500

        connection = java.sql.DriverManager.get_connection(JDBC_CONFIG)
        results = begin
          statement = connection.create_statement
          begin
            results = (1..queries).map do
              id = Random.rand(10000) + 1
              rs = statement.execute_query("SELECT * FROM World WHERE id = " + id.to_s)
              rs.next
              {id: rs.getObject('id'), randomNumber: rs.getObject('randomNumber')}
            end

            #mass update
            values = results.map { |h| ['(', h[:id], ',' ,Random.rand(10000) + 1, ')', ','] }.flatten[0..-2].join
            sql = "INSERT INTO `World` (`id`,`randomNumber`) VALUES #{values} ON DUPLICATE KEY UPDATE `World`.`randomNumber` = VALUES(`randomNumber`)"
            rs = statement.execute_update(sql)

            results
          ensure
            statement.close
          end
        ensure
          connection.close
        end

        ['application/json; charset=utf-8', results.to_json] 
      end
    [200, { 'Content-Type' => content_type }, [body]]
  end 
end