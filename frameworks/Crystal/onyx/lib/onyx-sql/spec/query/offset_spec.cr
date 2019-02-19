require "../models"

describe "Query#offset" do
  context "with int argument" do
    it do
      q = Query(User).new.offset(2)

      sql, params = q.build

      sql.should eq <<-SQL
      SELECT users.* FROM users OFFSET 2
      SQL

      params.should be_empty
    end
  end

  context "with nil argument" do
    it do
      q = Query(User).new.offset(nil)

      sql, params = q.build

      sql.should eq <<-SQL
      SELECT users.* FROM users
      SQL

      params.should be_empty
    end
  end
end
