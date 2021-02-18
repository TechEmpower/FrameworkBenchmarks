class WorldSerializer < BaseSerializer
  def initialize(@world : World)
  end

  def render
    {
      id:           @world.id,
      randomNumber: @world.randomnumber,
    }
  end
end
