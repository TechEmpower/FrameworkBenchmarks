require "../sqlite3_spec"
require "../../repository_spec"

describe "Repository(Postgres)#scalar" do
  repo = repo(:sqlite3)

  describe "with SQL" do
    context "without params" do
      it do
        repo.scalar("SELECT 1").as(Int64).should eq 1
      end
    end

    context "with single param" do
      it do
        repo.scalar("SELECT ?", 1).as(Int64).should eq 1
      end
    end
  end
end
