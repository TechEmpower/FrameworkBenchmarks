require "./models"

describe Model do
  describe "==" do
    it do
      uuid = UUID.random
      (User.new(uuid: uuid) == User.new(uuid: uuid)).should be_true
    end

    it do
      (User.new(uuid: UUID.random) == User.new(uuid: UUID.random)).should be_false
    end
  end
end
