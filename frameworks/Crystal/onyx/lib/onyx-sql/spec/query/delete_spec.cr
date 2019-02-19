require "../models"

describe "Query#delete" do
  it do
    uuid = UUID.random

    q = Query(User).new.delete.where(uuid: uuid)
    sql, params = q.build

    sql.should eq <<-SQL
    DELETE FROM users WHERE (users.uuid = ?)
    SQL

    params.to_a.should eq [uuid.to_s]
  end
end
