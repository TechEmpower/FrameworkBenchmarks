# frozen_string_literal: true

require 'oj'
Oj.mimic_JSON

# Our Rack application to be executed by rackup
class HelloWorld
  DEFAULT_HEADERS = {}.tap do |h|
    h[SERVER_HEADER] = SERVER_STRING if SERVER_STRING

    h.freeze
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
    DB.synchronize do
      ALL_IDS.sample(bounded_queries(env)).map do |id|
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
    DB.synchronize do
      worlds =
        ALL_IDS.sample(bounded_queries(env)).map do |id|
          world = World::BY_ID.(id: id)
          world[:randomnumber] = rand1
          world
        end
      World.batch_update(worlds)
      worlds
    end
  end

  def call(env)
    content_type, *body =
      case env['PATH_INFO']
      when '/json'
        # Test type 1: JSON serialization
        [JSON_TYPE, { message: 'Hello, World!' }.to_json]
      when '/db'
        # Test type 2: Single database query
        [JSON_TYPE, db.to_json]
      when '/queries'
        # Test type 3: Multiple database queries
        [JSON_TYPE, queries(env).to_json]
      when '/fortunes'
        # Test type 4: Fortunes
        [HTML_TYPE, fortunes]
      when '/updates'
        # Test type 5: Database updates
        [JSON_TYPE, updates(env).to_json]
      when '/plaintext'
        # Test type 6: Plaintext
        [PLAINTEXT_TYPE, 'Hello, World!']
      end

    [
      200,
      DEFAULT_HEADERS.merge(
        CONTENT_TYPE => content_type,
        DATE_HEADER => Time.now.httpdate
      ),
      body
    ]
  end
end
