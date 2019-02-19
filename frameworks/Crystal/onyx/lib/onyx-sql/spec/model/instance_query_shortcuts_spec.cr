require "../query/**"

describe Onyx::SQL::Model do
  describe "#insert" do
    it do
      User.new(name: "John").insert.build.should eq User.insert(name: "John", updated_at: nil, referrer: nil).build
    end
  end

  describe "#update" do
    uuid = UUID.random
    user = User.new(uuid: uuid, name: "John", active: true)
    changeset = user.changeset
    changeset.update(name: "Jake", active: false)

    it do
      user.update(changeset).build.should eq User.update.set(active: false, name: "Jake").where(uuid: uuid).build
    end
  end

  describe "#delete" do
    uuid = UUID.random
    user = User.new(uuid: uuid, name: "John")

    it do
      user.delete.build.should eq User.delete.where(uuid: uuid).build
    end
  end
end
