require "../models"

describe "Query#group_by" do
  it do
    q = Query(User).new.group_by("foo", "bar")
    sql, params = q.build

    sql.should eq <<-SQL
    SELECT users.* FROM users GROUP BY foo, bar
    SQL

    params.should be_empty
  end
end
