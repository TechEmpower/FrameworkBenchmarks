require "../models"

describe Onyx::SQL::Model do
  user = User.new(name: "John")
  changeset = user.changeset

  describe Onyx::SQL::Model::Changeset do
    it "is initially empty" do
      changeset.changes.empty?.should be_true
    end

    it "tracks changes" do
      changeset.update(name: "Bar")
      changeset.changes.should eq ({"name" => "Bar"})
    end
  end

  describe "#apply" do
    it do
      user.apply(changeset)
      user.name.should eq "Bar"
    end
  end
end
