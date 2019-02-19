require "../query/**"

describe Onyx::SQL::Model::ClassQueryShortcuts do
  describe ".query" do
    it do
      User.query.should eq Query(User).new
    end
  end

  describe ".group_by" do
    it do
      User.group_by("foo", "bar").should eq Query(User).new.group_by("foo", "bar")
    end
  end

  describe ".having" do
    it do
      User.having("foo").having("bar = ?", 42).build.should eq Query(User).new.having("foo").having("bar = ?", 42).build
    end
  end

  describe ".insert" do
    it do
      User.insert(name: "John").build.should eq Query(User).new.insert(name: "John").build
    end
  end

  describe ".limit" do
    it do
      User.limit(1).should eq Query(User).new.limit(1)
    end
  end

  describe ".offset" do
    it do
      User.offset(1).should eq Query(User).new.offset(1)
    end
  end

  describe ".set" do
    it do
      User.set(active: true).build.should eq Query(User).new.set(active: true).build
    end
  end

  describe ".where" do
    it do
      User.where(active: true).build.should eq Query(User).new.where(active: true).build
    end
  end

  {% for m in %w(update delete all one first last) %}
    describe {{m}} do
      it do
        User.{{m.id}}.should eq Query(User).new.{{m.id}}
      end
    end
  {% end %}

  describe ".join" do
    context "with table" do
      it do
        Post.join("users", "author.id = posts.author_id", as: "author").should eq Query(Post).new.join("users", "author.id = posts.author_id", as: "author")
      end
    end

    context "with reference" do
      it do
        Post.join(:author).should eq Query(Post).new.join(:author)
      end
    end
  end

  describe ".order_by" do
    it do
      User.order_by(:uuid, :asc).order_by("foo").should eq Query(User).new.order_by(:uuid, :asc).order_by("foo")
    end
  end

  describe ".returning" do
    it do
      User.returning(:name).returning("*").should eq Query(User).new.returning(:name).returning("*")
    end
  end

  describe ".select" do
    it do
      User.select(:name).select("*").should eq Query(User).new.select(:name).select("*")
    end
  end
end
