require "../models"

describe "Query#join" do
  context "explicit" do
    it do
      query = Query(User).new.join("some_table", "users.some_keys @> some_table.key", type: :right)

      sql, params = query.build

      sql.should eq <<-SQL
      SELECT users.* FROM users RIGHT JOIN some_table ON users.some_keys @> some_table.key
      SQL

      params.should be_empty
    end
  end

  context "with enumerable foreign reference" do
    it do
      query = Query(User).new.join(:authored_posts)

      sql, params = query.build

      sql.should eq <<-SQL
      SELECT users.* FROM users INNER JOIN posts AS authored_posts ON users.uuid = authored_posts.author_uuid
      SQL

      params.should be_empty
    end

    context "with nested query" do
      it do
        query = Query(User).new
          .join(authored_posts: true) do |q|
            q.select(:content)
            q.where(content: "foo")
          end

        sql, params = query.build

        # Foreign enumerable references' selects are ommited
        sql.should eq <<-SQL
        SELECT users.* FROM users INNER JOIN posts AS authored_posts ON users.uuid = authored_posts.author_uuid WHERE (authored_posts.content = ?)
        SQL

        params.to_a.should eq ["foo"]
      end
    end

    context "with deep nested query" do
      it do
        uuid = UUID.random

        query = Query(User).new
          .join authored_posts: true, type: Query::JoinType::Right do |q|
            q.select(:content)
            q.where(content: "foo")

            q.join editor: true, as: "the_editor" do |qq|
              qq.select(:name)
              qq.where(uuid: uuid)
            end

            q.join tags: true do |qq|
              qq.where("tags.content LIKE %foo%")
            end
          end

        sql, params = query.build

        # ditto
        sql.should eq <<-SQL
        SELECT users.* FROM users RIGHT JOIN posts AS authored_posts ON users.uuid = authored_posts.author_uuid INNER JOIN users AS the_editor ON the_editor.uuid = authored_posts.editor_uuid INNER JOIN tags AS tags ON tags.id IN authored_posts.tag_ids WHERE (authored_posts.content = ?) AND (the_editor.uuid = ?) AND (tags.content LIKE %foo%)
        SQL

        params.to_a.should eq ["foo", uuid.to_s]
      end
    end

    context "with additional arguments" do
      it do
        query = Query(User).new.join(:authored_posts, type: :right, as: "the_posts")

        query.to_s.should eq <<-SQL
        SELECT users.* FROM users RIGHT JOIN posts AS the_posts ON users.uuid = the_posts.author_uuid
        SQL
      end
    end
  end

  context "with foreign reference with self referenced as enumerable" do
    it do
      query = Query(Tag).new.join(:posts)

      query.to_s.should eq <<-SQL
      SELECT tags.* FROM tags INNER JOIN posts AS posts ON tags.id IN posts.tag_ids
      SQL
    end
  end

  context "with direct reference" do
    it do
      query = Query(Post).new.join(:author, type: :left)

      query.to_s.should eq <<-SQL
      SELECT posts.* FROM posts LEFT JOIN users AS author ON author.uuid = posts.author_uuid
      SQL
    end
  end

  context "with enumerable direct reference" do
    it do
      query = Query(Post).new.join(:tags)

      query.to_s.should eq <<-SQL
      SELECT posts.* FROM posts INNER JOIN tags AS tags ON tags.id IN posts.tag_ids
      SQL
    end
  end
end
