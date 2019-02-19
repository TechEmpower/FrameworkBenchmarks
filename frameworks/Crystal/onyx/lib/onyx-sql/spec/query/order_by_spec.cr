require "../models"

describe "Query#order_by" do
  context "with attribute argument" do
    it do
      q = Query(User).new.order_by(:active, :desc)

      sql, params = q.build

      sql.should eq <<-SQL
      SELECT users.* FROM users ORDER BY users.activity_status DESC
      SQL

      params.should be_empty
    end
  end

  context "with string argument" do
    it do
      q = Query(User).new.order_by("some_column")

      sql, params = q.build

      sql.should eq <<-SQL
      SELECT users.* FROM users ORDER BY some_column
      SQL

      params.should be_empty
    end
  end
end
