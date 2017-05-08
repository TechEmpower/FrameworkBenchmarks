module App
  Ruby = lambda do |env|
    content_type, body = case env['PATH_INFO']
      when '/plaintext'
        ['text/plain', "Hello, World!"]
      when '/json'
        ['application/json', {:message => "Hello, World!"}.to_json]
      when '/db'
        id = Random.rand(10000) + 1
        query = "SELECT * FROM World WHERE id = " + id.to_s

        client = Mysql2::Client.new(DB_CONFIG)
        results = begin
          client.query(query)
        ensure
          client.close
        end
        ['application/json', results.first.to_json]
      when '/queries'
        query_string = Rack::Utils.parse_query(env['QUERY_STRING'])
        queries = query_string['queries'].to_i
        queries = 1 if queries < 1
        queries = 500 if queries > 500

        client = Mysql2::Client.new(DB_CONFIG)
        results = begin
          (1..queries).map do
            id = Random.rand(10000) + 1
            client.query("SELECT * FROM World WHERE id = " + id.to_s).first
          end
        ensure
          client.close
        end
        ['application/json', results.to_json]
      when '/updates'
        query_string = Rack::Utils.parse_query(env['QUERY_STRING'])
        queries = query_string['queries'].to_i
        queries = 1 if queries < 1
        queries = 500 if queries > 500

        client = Mysql2::Client.new(DB_CONFIG)
        results = begin
          results = (1..queries).map do
            id = Random.rand(10000) + 1
            num = Random.rand(10000) + 1
            client.query("UPDATE World SET randomNumber = " + num.to_s + " WHERE id = " + id.to_s)
            client.query("SELECT * FROM World WHERE id = " + id.to_s).first
          end

          results
        ensure
          client.close
        end
        ['application/json', results.to_json]
      end
    [200, { 'Content-Type' => content_type, 'Date' => Time.now.httpdate, 'Server' => 'WebServer' }, [body]]
  end
end
