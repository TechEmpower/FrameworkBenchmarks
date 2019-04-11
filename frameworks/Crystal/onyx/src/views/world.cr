struct Views::World
  include Onyx::HTTP::View

  def initialize(@world : Models::World)
  end

  json do
    object do
      field "id", @world.id
      field "randomnumber", @world.random_number
    end
  end
end
