struct Views::Worlds
  include Onyx::HTTP::View

  def initialize(@worlds : Enumerable(Models::World))
  end

  json @worlds.map { |world| Views::World.new(world) }
end
