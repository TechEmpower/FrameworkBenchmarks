# frozen_string_literal: true

require 'rack/app'
require 'rack/app/front_end'
require 'json'

class App < Rack::App
  MAX_PK = 10_000
  ID_RANGE = (1..10_000).freeze
  ALL_IDS = ID_RANGE.to_a
  QUERIES_MIN = 1
  QUERIES_MAX = 500
  JSON_TYPE = 'application/json'
  HTML_TYPE = 'text/html; charset=utf-8'
  PLAINTEXT_TYPE = 'text/plain'

  apply_extensions :front_end

  helpers do
    def fortunes
      fortunes = Fortune.all
      fortunes << Fortune.new(
        id: 0,
        message: "Additional fortune added at request time."
      )
      fortunes.sort_by!(&:message)
    end
  end

  get '/json' do
    set_headers(JSON_TYPE)
    { message: 'Hello, World!' }.to_json
  end

  get '/db' do
    set_headers(JSON_TYPE)
    World.with_pk(rand1).values.to_json
  end

  get '/queries' do
    set_headers(JSON_TYPE)
    ids = ALL_IDS.sample(bounded_queries)
    DB.synchronize do
      ids.map do |id|
        World.with_pk(id).values
      end
    end.to_json
  end

  get '/fortunes' do
    set_headers(HTML_TYPE)
    render 'fortunes.html.erb'
  end

  get '/plaintext' do
    set_headers(PLAINTEXT_TYPE)
    'Hello, World!'
  end

  private

  # Return a random number between 1 and MAX_PK
  def rand1
    rand(MAX_PK).succ
  end

  def bounded_queries
    queries = params['queries'].to_i
    queries.clamp(QUERIES_MIN, QUERIES_MAX)
  end

  def set_headers(content_type)
    response.headers[::Rack::CONTENT_TYPE] = content_type
    response.headers['Server'] = 'rack-app'
  end
end
