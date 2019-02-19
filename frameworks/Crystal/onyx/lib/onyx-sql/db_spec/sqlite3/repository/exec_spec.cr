require "../sqlite3_spec"
require "../../repository_spec"

describe "Repository(Postgres)#exec" do
  repo = repo(:sqlite3)

  describe "with SQL" do
    context "without params" do
      it do
        repo.exec("SELECT 1").should be_truthy
      end
    end

    context "with single param" do
      it do
        repo.exec("SELECT ?", 1).should be_truthy
      end
    end

    context "with multiple params" do
      it do
        repo.exec("SELECT ?, ?", 1, "foo").should be_truthy
      end
    end
  end
end
