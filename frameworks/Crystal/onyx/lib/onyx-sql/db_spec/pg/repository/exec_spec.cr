require "../pg_spec"
require "../../repository_spec"

describe "Repository(Postgres)#exec" do
  repo = repo(:postgresql)

  describe "with SQL" do
    context "without params" do
      it do
        repo.exec("SELECT 1").should be_truthy
      end
    end

    context "with single param" do
      it do
        repo.exec("SELECT ?::int", 1).should be_truthy
      end
    end

    context "with multiple params" do
      it do
        repo.exec("SELECT ?::int, ?::text", 1, "foo").should be_truthy
      end
    end

    context "with single array of params" do
      it do
        repo.exec("SELECT ?::int[]", "{1,2}").should be_truthy
      end
    end

    context "with multiple arguments which have an array of params" do
      it do
        repo.exec("SELECT ?::text, ?::int[]", "foo", "{1,2}").should be_truthy
      end
    end
  end
end
