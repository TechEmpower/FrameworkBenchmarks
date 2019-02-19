require "../models"

describe "Query#limit" do
  context "with int argument" do
    it do
      q = Query(User).new.limit(2)

      sql, params = q.build

      sql.should eq <<-SQL
      SELECT users.* FROM users LIMIT 2
      SQL

      params.should be_empty
    end
  end

  context "with nil argument" do
    it do
      q = Query(User).new.limit(nil)

      sql, params = q.build

      sql.should eq <<-SQL
      SELECT users.* FROM users
      SQL

      params.should be_empty
    end
  end
end
