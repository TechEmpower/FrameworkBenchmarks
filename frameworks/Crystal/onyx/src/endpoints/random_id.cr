module Endpoints::RandomID
  def random_id
    Random.rand(10_000).succ
  end
end
