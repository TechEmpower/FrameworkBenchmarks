require "../models"

describe "Query#select" do
  context "without call" do
    it do
      q = Query(User).new

      sql, params = q.build

      sql.should eq <<-SQL
      SELECT users.* FROM users
      SQL

      params.should be_empty
    end
  end

  context "with call" do
    it do
      q = Query(User).new.select(:active, "foo")

      sql, params = q.build

      sql.should eq <<-SQL
      SELECT users.activity_status, foo FROM users
      SQL

      params.should be_empty
    end
  end
end
