require "../models"

describe "Query#insert" do
  context "with minimum arguments" do
    it do
      q = Query(User).new.insert(name: "John")

      sql, params = q.build

      sql.should eq <<-SQL
      INSERT INTO users (name) VALUES (?)
      SQL

      params.to_a.should eq ["John"]
    end
  end

  context "with many arguments" do
    it do
      ref_uuid = UUID.random

      q = Query(User).new.insert(
        referrer: User.new(uuid: ref_uuid, name: "Jake"),
        role: User::Role::Moderator,
        permissions: [User::Permission::CreatePosts, User::Permission::EditPosts],
        name: "John",
        favorite_numbers: [3, 17, 42]
      )

      sql, params = q.build

      sql.should eq <<-SQL
      INSERT INTO users (referrer_uuid, role, permissions, name, favorite_numbers) VALUES (?, ?, ?, ?, ?)
      SQL

      params.to_a.should eq [ref_uuid.to_s, "Moderator", "CreatePosts,EditPosts", "John", "3,17,42"]
    end
  end
end
