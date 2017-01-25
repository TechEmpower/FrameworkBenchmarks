# frozen_string_literal: true
require 'bundler'
require 'time'

MAX_PK = 10_000
QUERIES_MIN = 1
QUERIES_MAX = 500
SEQUEL_NO_ASSOCIATIONS = true

Bundler.require(:default) # Load core modules

def connect(dbtype)
  Bundler.require(dbtype) # Load database-specific modules

  adapters = {
    :mysql=>{ :jruby=>'jdbc:mysql', :mri=>'mysql2' },
    :postgresql=>{ :jruby=>'jdbc:postgresql', :mri=>'postgres' }
  }

  opts = {}

  # Determine threading/thread pool size and timeout
  if defined?(JRUBY_VERSION)
    opts[:max_connections] = Integer(ENV.fetch('MAX_CONCURRENCY'))
    opts[:pool_timeout] = 10
  elsif defined?(Puma)
    opts[:max_connections] = Puma.cli_config.options.fetch(:max_threads)
    opts[:pool_timeout] = 10
  else
    Sequel.single_threaded = true
  end

  Sequel.connect \
    '%<adapter>s://%<host>s/%<database>s?user=%<user>s&password=%<password>s' % {
      :adapter=>adapters.fetch(dbtype).fetch(defined?(JRUBY_VERSION) ? :jruby : :mri),
      :host=>ENV.fetch('DBHOST', '127.0.0.1'),
      :database=>'hello_world',
      :user=>'benchmarkdbuser',
      :password=>'benchmarkdbpass'
    }, opts
end

DB = connect(ENV.fetch('DBTYPE').to_sym).tap do |db|
  db.extension(:freeze_datasets)
  db.freeze
end

# Define ORM models
class World < Sequel::Model(:World)
  def_column_alias(:randomnumber, :randomNumber) if DB.database_type == :mysql
end

class Fortune < Sequel::Model(:Fortune)
  # Allow setting id to zero (0) per benchmark requirements
  unrestrict_primary_key
end

# Our Rack application to be executed by rackup
class HelloWorld
  DEFAULT_HEADERS = {}.tap do |h|
    server_string =
      if defined?(PhusionPassenger)
        [
          PhusionPassenger::SharedConstants::SERVER_TOKEN_NAME,
          PhusionPassenger::VERSION_STRING
        ].join('/').freeze
      elsif defined?(Puma)
        Puma::Const::PUMA_SERVER_STRING
      elsif defined?(Unicorn)
        Unicorn::HttpParser::DEFAULTS['SERVER_SOFTWARE']
      end

    h['Server'] = server_string if server_string
  end.freeze

  def bounded_queries(env)
    params = Rack::Utils.parse_query(env['QUERY_STRING'])

    queries = params['queries'].to_i
    return QUERIES_MIN if queries < QUERIES_MIN
    return QUERIES_MAX if queries > QUERIES_MAX
    queries
  end

  # Return a random number between 1 and MAX_PK
  def rand1
    Random.rand(MAX_PK).succ
  end

  # Return an array of `n' unique random numbers between 1 and MAX_PK
  def randn(n)
    (1..MAX_PK).to_a.shuffle!.take(n)
  end

  def db
    World.with_pk(rand1).values
  end

  def queries(env)
    # Benchmark requirements explicitly forbid a WHERE..IN here, so be good
    randn(bounded_queries(env))
      .map! { |id| World.with_pk(id).values }
  end

  def fortunes
    fortunes = Fortune.all
    fortunes << Fortune.new(
      :id=>0,
      :message=>'Additional fortune added at request time.'
    )
    fortunes.sort_by!(&:message)

    html = <<~'HTML'
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
      html += <<~"HTML"
      <tr>
        <td>#{Rack::Utils.escape_html(fortune.id)}</td>
        <td>#{Rack::Utils.escape_html(fortune.message)}</td>
      </tr>
      HTML
    end

    html += <<~'HTML'
      </table>

      </body>
      </html>
    HTML
  end

  WORLD_BY_ID_FOR_UPDATE = World.naked.for_update.where(:id=>:$id).prepare(:first, :world_by_id_for_update)
  WORLD_UPDATE = World.where(:id=>:$id).prepare(:update, :world_update, :randomnumber=>:$randomnumber)

  def updates(env)
    # Benchmark requirements explicitly forbid a WHERE..IN here, transactions
    # are optional, batch updates are allowed (but each transaction can only
    # read and write a single record?), so... be good
    randn(bounded_queries(env)).map! do |id|
      DB.transaction do
        world = WORLD_BY_ID_FOR_UPDATE.(:id=>id)
        WORLD_UPDATE.(:id=>id, :randomnumber=>(world[:randomnumber] = rand1))
        world
      end
    end
  end

  def call(env)
    content_type, *body =
      case env['PATH_INFO']
      when '/json'
        # Test type 1: JSON serialization
        ['application/json', JSON.fast_generate(:message=>'Hello, World!')]
      when '/db'
        # Test type 2: Single database query
        ['application/json', JSON.fast_generate(db)]
      when '/queries'
        # Test type 3: Multiple database queries
        ['application/json', JSON.fast_generate(queries(env))]
      when '/fortunes'
        # Test type 4: Fortunes
        ['text/html; charset=utf-8', fortunes]
      when '/updates'
        # Test type 5: Database updates
        ['application/json', JSON.fast_generate(updates(env))]
      when '/plaintext'
        # Test type 6: Plaintext
        ['text/plain', 'Hello, World!']
      end

    return 200,
      DEFAULT_HEADERS.merge(
        'Content-Type'=>content_type,
        'Date'=>Time.now.httpdate
      ),
      body
  end
end
