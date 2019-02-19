require "../pg_spec"
require "../../repository_spec"

describe "Repository(Postgres)#scalar" do
  repo = repo(:postgresql)

  describe "with SQL" do
    context "without params" do
      it do
        repo.scalar("SELECT 1").as(Int32).should eq 1
      end
    end

    context "with single param" do
      it do
        repo.scalar("SELECT ?::int", 1).as(Int32).should eq 1
      end
    end

    context "with array of params" do
      it do
        repo.scalar("SELECT ?::int[]", "{1,2}").as(Array(PG::Int32Array)).should eq [1, 2]
      end
    end
  end
end
