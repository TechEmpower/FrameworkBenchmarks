require "../pg_spec"
require "../../repository_spec"
require "../models"

describe "Repository(Postgres)#query" do
  repo = repo(:postgresql)

  # This is John
  #

  user = uninitialized User

  describe "insert" do
    context "with a simple model" do
      user = User.new(
        name: "John",
        active: true,
        favorite_numbers: [3, 17]
      )

      user = repo.query(user.insert.returning("*")).first

      it "returns instance" do
        user.should be_a(User)
        user.favorite_numbers.should eq [3, 17]
      end
    end
  end

  # And this is John's referrer, Jake
  #

  referrer = repo.query(User.insert(name: "Jake").returning(User)).first

  describe "update" do
    context "with attributes" do
      previous_uuid = user.uuid

      # We're updating John's balance and activity status
      #

      query = User.query
        .update
        .set(active: false)
        .set(balance: 100.0_f32, updated_at: nil)
        .set(favorite_numbers: [11])
        .where(uuid: user.uuid)
        .returning(:uuid, :balance)

      user = repo.query(query).first

      it "preloads attributes" do
        user.uuid.should eq previous_uuid
        user.balance.should eq 100.0
      end
    end

    context "with direct references" do
      # We're setting John's referrer to Jake
      #

      query = User.update.set(referrer: referrer).where(uuid: user.uuid).returning(User)
      user = repo.query(query).first

      it "preloads references" do
        user.referrer.not_nil!.uuid.should be_a(UUID)
      end
    end
  end

  describe "where" do
    user = repo.query(User.where(uuid: user.uuid).and_where(balance: 100.0_f32)).first

    it "returns a User instance" do
      user.name.should eq "John"
    end

    context "with direct non-enumerable join" do
      query = User.query
        .select(:name, :uuid)
        .where(uuid: user.uuid)
        .join referrer: true do |q|
          q.select("referrer.*")
        end

      user = repo.query(query).first

      it "returns a User instance" do
        user.name.should eq "John"
      end

      it "preloads direct references" do
        user.referrer.not_nil!.name.should eq "Jake"
      end
    end
  end

  tag = repo.query(Tag.insert(content: "foo").returning("*")).first
  post = uninitialized Post

  describe "insert" do
    context "with complex model" do
      post = repo.query(Post.query
        .insert(author: user, tags: [tag], content: "Blah-blah")
        .returning(Post)
      ).first

      it "returns model instance" do
        post.should be_a(Post)
      end

      it "preloads direct non-enumerable references" do
        post.author.uuid.should eq user.uuid
        post.author.name?.should be_nil
      end

      it "preloads direct enumerable references" do
        post.tags.size.should eq 1
        post.tags.first.id.should eq tag.id
        post.tags.first.content?.should be_nil
      end
    end
  end

  new_user = repo.query(User.insert(name: "James").returning(User)).first

  describe "update" do
    context "with complex reference updates" do
      changeset = post.changeset

      changeset.update(tags: [] of Tag, editor: new_user)
      changeset.update(created_at: Time.now)

      post = repo.query(post.update(changeset).returning("*")).first

      it "returns model instance" do
        post.should be_a(Post)
      end

      it "preloads direct non-enumerable references" do
        post.editor.not_nil!.uuid.should eq new_user.uuid
      end

      it "preloads direct enumerable references" do
        post.tags.size.should eq 0
      end
    end
  end

  describe "where" do
    context "with foreign non-enumerable join" do
      post = repo.query(Post.query
        .where(id: post.id)
        .and("cardinality(tag_ids) = ?", 0)

        .join author: true do |q|
          q.select("author.*")
        end

        .join editor: true do |q|
          q.select(:uuid)
        end
      ).first

      it "returns model instance" do
        post.should be_a(Post)
      end

      it "preloads references" do
        post.author.uuid.should eq user.uuid
        post.editor.not_nil!.uuid.should eq new_user.uuid
      end
    end
  end
end
