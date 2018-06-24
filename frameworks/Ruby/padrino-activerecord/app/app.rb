module PadrinoActiverecord
  class App < Padrino::Application
    use ConnectionPoolManagement
    register Padrino::Helpers

    enable :sessions
    disable :static
    disable :protection

    set :add_charset, [mime_type(:html)]

    helpers do
      def bounded_queries
        queries = params[:queries].to_i
        return QUERIES_MIN if queries < QUERIES_MIN
        return QUERIES_MAX if queries > QUERIES_MAX
        queries
      end

      def json(data)
        content_type :json
        JSON.fast_generate(data)
      end

      # Return a random number between 1 and MAX_PK
      def rand1
        rand(MAX_PK).succ
      end
    end

    after do
      response['Date'] = Time.now.httpdate
    end

    after do
      response['Server'] = SERVER_STRING
    end if SERVER_STRING

    # Test type 1: JSON serialization
    get '/json' do
       json :message=>'Hello, World!'
    end

    # Test type 2: Single database query
    get '/db' do
      world =
        ActiveRecord::Base.connection_pool.with_connection do
          World.find(rand1).attributes
        end

      json world
    end

    # Test type 3: Multiple database queries
    get '/queries' do
      worlds =
        ActiveRecord::Base.connection_pool.with_connection do
          Array.new(bounded_queries) do
            World.find(rand1)
          end
        end

      json worlds.map!(&:attributes)
    end

    # Test type 4: Fortunes
    get '/fortunes' do
      @fortunes = ActiveRecord::Base.connection_pool.with_connection do
        Fortune.all
      end.to_a
      @fortunes << Fortune.new(
        :id=>0,
        :message=>'Additional fortune added at request time.'
      )
      @fortunes.sort_by!(&:message)

      erb :fortunes, :layout=>true
    end

    # Test type 5: Database updates
    get '/updates' do
      worlds =
        ActiveRecord::Base.connection_pool.with_connection do
          Array.new(bounded_queries) do
            world = World.find(rand1)
            world.update(:randomnumber=>rand1)
            world
          end
        end

      json worlds.map!(&:attributes)
    end

    # Test type 6: Plaintext
    get '/plaintext' do
      content_type :text
      'Hello, World!'
    end

    ##
    # Caching support.
    #
    # register Padrino::Cache
    # enable :caching
    #
    # You can customize caching store engines:
    #
    # set :cache, Padrino::Cache.new(:LRUHash) # Keeps cached values in memory
    # set :cache, Padrino::Cache.new(:Memcached) # Uses default server at localhost
    # set :cache, Padrino::Cache.new(:Memcached, :server => '127.0.0.1:11211', :exception_retry_limit => 1)
    # set :cache, Padrino::Cache.new(:Memcached, :backend => memcached_or_dalli_instance)
    # set :cache, Padrino::Cache.new(:Redis) # Uses default server at localhost
    # set :cache, Padrino::Cache.new(:Redis, :host => '127.0.0.1', :port => 6379, :db => 0)
    # set :cache, Padrino::Cache.new(:Redis, :backend => redis_instance)
    # set :cache, Padrino::Cache.new(:Mongo) # Uses default server at localhost
    # set :cache, Padrino::Cache.new(:Mongo, :backend => mongo_client_instance)
    # set :cache, Padrino::Cache.new(:File, :dir => Padrino.root('tmp', app_name.to_s, 'cache')) # default choice
    #

    ##
    # Application configuration options.
    #
    # set :raise_errors, true       # Raise exceptions (will stop application) (default for test)
    # set :dump_errors, true        # Exception backtraces are written to STDERR (default for production/development)
    # set :show_exceptions, true    # Shows a stack trace in browser (default for development)
    set :logging, false            # Logging in STDOUT for development and file for production (default only for development)
    # set :public_folder, 'foo/bar' # Location for static assets (default root/public)
    # set :reload, false            # Reload application files (default in development)
    # set :default_builder, 'foo'   # Set a custom form builder (default 'StandardFormBuilder')
    # set :locale_path, 'bar'       # Set path for I18n translations (default your_apps_root_path/locale)
    # disable :sessions             # Disabled sessions by default (enable if needed)
    # disable :flash                # Disables sinatra-flash (enabled by default if Sinatra::Flash is defined)
    layout  :layout            # Layout can be in views/layouts/foo.ext or views/foo.ext (default :application)
    #

    ##
    # You can configure for a specified environment like:
    #
    #   configure :development do
    #     set :foo, :bar
    #     disable :asset_stamp # no asset timestamping for dev
    #   end
    #

    ##
    # You can manage errors like:
    #
    #   error 404 do
    #     render 'errors/404'
    #   end
    #
    #   error 500 do
    #     render 'errors/500'
    #   end
    #
  end
end
