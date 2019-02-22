require "pg"
require "onyx/sql"
require "onyx/rest"

class World
  include Onyx::SQL::Model

  schema world do
    pkey id : Int32
    type random_number : Int32, key: "randomnumber"
  end
end

def random_id
  Random.rand(10_000).succ
end

struct Views::World
  include Onyx::REST::View

  def initialize(@world : ::World)
  end

  json do
    object do
      field "id", @world.id
      field "randomnumber", @world.random_number
    end
  end
end

struct Views::Worlds
  include Onyx::REST::View

  def initialize(@worlds : Enumerable(::World))
  end

  json do
    array do
      @worlds.each do |world|
        Views::World.new(world).to_json(itself)
      end
    end
  end
end

struct Actions::RandomWorld
  include Onyx::REST::Action

  QUERY = World.where(id: 0).build(true)[0]

  def call
    world = Onyx.query(World, QUERY, random_id).first
    return Views::World.new(world)
  end
end

struct Actions::ManyWorlds
  include Onyx::REST::Action

  QUERY = World.where("id = ANY(?)", 0).build(true)[0]

  params do
    query do
      type queries : String | Int32 = 1
    end
  end

  def call
    queries = params.query.queries.is_a?(String) ? 1 : params.query.queries.as(Int32)
    ids = queries.clamp(1..500).times.map do
      random_id
    end.join(',').try do |s|
      "{#{s}}"
    end

    worlds = Onyx.query(World, QUERY, ids)
    return Views::Worlds.new(worlds)
  end
end

struct Actions::UpdateWorlds
  include Onyx::REST::Action

  QUERY = World.update.set(random_number: 0).where(id: 0).build[0]

  params do
    query do
      type queries : String | Int32 = 1
    end
  end

  def call
    queries = params.query.queries.is_a?(String) ? 1 : params.query.queries.as(Int32)

    worlds = Array(World).new

    queries.clamp(1..500).times.each do
      id, number = {random_id, random_id}

      world = World.new(id: id, random_number: number)
      worlds << world

      Onyx.exec(QUERY, world.random_number, world.id)
    end

    return Views::Worlds.new(worlds)
  end
end

class CustomHandler
  include HTTP::Handler

  def call(context)
    context.response.headers["Server"] = "Onyx"
    context.response.headers["Date"] = HTTP.format_time(Time.now)
    call_next(context)
  end
end

Onyx.draw do
  get "/json" do |env|
    env.response.content_type = "application/json"
    {message: "Hello, World!"}.to_json(env.response)
  end

  get "/db", Actions::RandomWorld
  get "/queries", Actions::ManyWorlds
  get "/updates", Actions::UpdateWorlds

  get "/plaintext" do |env|
    env.response.content_type = "text/plain"
    env.response << "Hello, World!"
  end
end

Onyx.render(:json)

Onyx.listen(ENV["TEST_HOST"], 8080) do
  handlers.insert(2, CustomHandler.new)
end
