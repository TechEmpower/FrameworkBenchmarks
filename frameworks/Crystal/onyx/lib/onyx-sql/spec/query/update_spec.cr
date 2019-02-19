require "../models"

describe "Query#update" do
  it do
    uuid = UUID.random
    ref_uuid = UUID.random

    q = Query(User).new
      .update
      .set(name: "John")
      .set("activity_status = DEFAULT")
      .set(referrer: User.new(
      uuid: ref_uuid,
      name: "Jake"
    ), updated_at: nil, favorite_numbers: [0, 1])
      .where(uuid: uuid)

    sql, params = q.build

    sql.should eq <<-SQL
    UPDATE users SET name = ?, activity_status = DEFAULT, referrer_uuid = ?, updated_at = ?, favorite_numbers = ? WHERE (users.uuid = ?)
    SQL

    params.to_a.should eq ["John", ref_uuid.to_s, nil, "0,1", uuid.to_s]
  end
end
