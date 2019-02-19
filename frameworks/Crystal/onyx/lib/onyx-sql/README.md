<a href="https://onyxframework.org"><img width="100" height="100" src="https://onyxframework.org/img/logo.svg"></a>

# Onyx::SQL

[![Built with Crystal](https://img.shields.io/badge/built%20with-crystal-000000.svg?style=flat-square)](https://crystal-lang.org/)
[![Travis CI build](https://img.shields.io/travis/onyxframework/sql/master.svg?style=flat-square)](https://travis-ci.org/onyxframework/sql)
[![API docs](https://img.shields.io/badge/api_docs-online-brightgreen.svg?style=flat-square)](https://api.onyxframework.org/sql)
[![Latest release](https://img.shields.io/github/release/onyxframework/sql.svg?style=flat-square)](https://github.com/onyxframework/sql/releases)

An MIT-licensed SQL ORM for [Crystal](https://crystal-lang.org).

## Supporters ❤️

Thanks to all my patrons, I can continue working on beautiful Open Source Software! 🙏

[Alexander Maslov](https://seendex.ru), [Lauri Jutila](https://github.com/ljuti)

*You can become a patron too in exchange of prioritized support and other perks*

<a href="https://www.patreon.com/vladfaust"><img height="50" src="https://onyxframework.org/img/patreon-button.svg"></a>

## About 👋

Onyx::SQL is an SQL ORM for the [Crystal Language](https://crystal-lang.org). It features handy schema definition DSL and powerful type-safe query builder. It preserves composition and has a decent API documentation.

It is a part of [Onyx Framework](https://onyxframework.org), but it is **not** strictly tied to it. You absolutely can use this ORM with a web framework other than [Onyx::HTTP](https://github.com/onyxframework/http) and [Onyx::REST](https://github.com/onyxframework/rest).

It implements the [crystal-db](https://github.com/crystal-lang/crystal-db) API, which makes it usable with any SQL database! It has been successfully tested with the following DBs:

- [x] SQLite3
- [x] PostgreSQL
- [ ] MySQL (*coming soon*)

This ORM, as all other Onyx components, targets to be easily understandble for newcomers, but be able to grow with a developers's knowledge. Fundamentally, it relies on extremely powerful Crystal annotations, but they may be tedious for daily tasks, that why they're hidden by default under the convenient schema DSL. See the examples below.

## Installation 📥

Add this to your application's `shard.yml`:

```yaml
dependencies:
  onyx-sql:
    github: onyxframework/sql
    version: ~> 0.6.1
```

This shard follows [Semantic Versioning v2.0.0](http://semver.org/), so check [releases](https://github.com/onyxframework/sql/releases) and change the `version` accordingly. Please visit [github.com/crystal-lang/shards](https://github.com/crystal-lang/shards) to know more about Crystal shards.

You'd also need to add a database dependency conforming the [crystal-db](https://github.com/crystal-lang/crystal-db) interface. For example, [pg](https://github.com/will/crystal-pg):

```yaml
dependencies:
  onyx-sql:
    github: onyxframework/sql
    version: ~> 0.6.0
  pg:
    github: will/crystal-pg
    version: ~> 0.15.0
```

## Usage 💻

The API docs are hosted at <https://api.onyxframework.org/sql>, and they're pretty comprehensive. Don't hesistate to read them all after you're done with this section!

It's a good idea to get yourself familiar with the [Crystal DB docs](https://crystal-lang.org/reference/database/) before moving on.

### 101 📖

As any other ORM, Onyx::SQL allows to define models which will be mapped to SQL tables. Assuming that you have the following table in a PostgreSQL database:

```sql
CREATE TABLE users (
  id    SERIAL  PRIMARY KEY,
  name  TEXT    NOT NULL
);
```

> **Note:** Onyx::SQL does not provide any tools for migrations. Check [migrate.cr](https://github.com/vladfaust/migrate.cr) for a production-ready solution.

Then in your code you would do:

```crystal
require "pg"
require "onyx-sql"

class User
  include Onyx::SQL::Model

  schema users do
    pkey id : Int32
    type name : String
  end
end

db = DB.open("postgresql://postgres:postgres@localhost:5432/my_db")

user = User.new(name: "John")
rs = db.query(*user.insert.returning(User).build(true))

users = User.from_rs(rs)
user = users.first

pp user.id # => 1
```

Congratulations, you've successfully inserted a brand new User instance! :tada:

> **Note:** `.build(true)` is required instead of simple `.build` because PostgreSQL has different syntax for query arguments (`$n` instead of `?`).

#### Querying

```crystal
rs = db.query("SELECT * FROM users WHERE id = $1", 1)
user = User.from_rs(rs).first

pp user # => <User @id=1 @name="John">
```

#### Updating

You can easily update models with the [`Changeset`](https://api.onyxframework.org/sql/Onyx/SQL/Model/Changeset.html) concept:

```crystal
changeset = user.changeset
changeset.update(name: "Jake")

db.exec(*user.update(changeset).build(true))
```

#### Deletion

Deleting from DB is simple as well:

```crystal
db.exec(*user.delete.build(true))
```

### Repository

Onyx::SQL has the [`Onyx::SQL::Repository`](https://api.onyxframework.org/sql/Onyx/SQL/Repository.html) class, which effectively wraps the database connection with logging, automatically builds queries and more:

```crystal
repo = Onyx::SQL::Repository.new(db)

user = User.new(name: "Archer")
user = repo.query(user.insert.returning(User)).first

# [postgresql] INSERT INTO users (name) VALUES (?)
# 1.234ms
```

### Query

[`Onyx::SQL::Query`](https://api.onyxframework.org/sql/Onyx/SQL/Query.html) is a powerful type-safe SQL query builder with almost all SQL methods implemented:

```crystal
query = User.select(:name).where(id: 2)
pp query       # => <Onyx::SQL::Query(User) ...>
pp query.build # => {"SELECT users.name FROM users WHERE id = ?", {2}}
```

`Query` is just an object which could be expanded into a pair of SQL string a query params. You can then use it however you want:

```crystal
sql, params = query.build(true)
rs = db.query(sql, params)

# Or shorter
rs = db.query(*query.build(true))

# Or with repository
user = repo.query(query).first
```

### References

Onyx::SQL has a native support for model references, both direct and foreign ones.

```sql
CREATE TABLE posts (
  id          SERIAL      PRIMARY KEY,
  author_id   INT         NOT NULL  REFERENCES users (id),
  content     TEXT        NOT NULL,
  created_at  TIMESTAMPTZ NOT NULL  DEFAULT now()
);
```

```crystal
class User
  include Onyx::SQL::Model

  schema users do
    pkey id : Int32
    type name : String
    type authored_posts : Array(Post), foreign_key: "author_id"
  end
end

class Post
  include Onyx::SQL::Model

  schema posts do
    pkey id : Int32
    type content : String
    type author : User, key: "author_id"
  end
end

user = User.new(id: 2, name: "Archer")
post = Post.new(content: "Classic", author: user)
repo.exec(post.insert)

# [postgresql] INSERT INTO posts (content, author_id) VALUES (?, ?)
# The actual DB arguments are "Classic" and 2
```

Thanks to the `Query` builder, it is possible to build powerful type-safe joins in no time:

```crystal
posts = repo.query(Post
  .select(:id, :content)
  .join(author: true) do |q|
    q.select(:id, :name)
    q.where(name: "Archer")
  end)

pp posts.first # <Post @id=1 @content="Classic" @author=<User @id=2 @name="Archer">>
```

### Macros

[Onyx top-level macros](https://github.com/onyxframework/onyx#sql) allow to define top-level repository methods:

```crystal
require "onyx/env"
require "onyx/sql"

Onyx.query  # Singleton Onyx::SQL::Repository.query call
Onyx.exec   # ditto
Onyx.scalar # ditto
```

### Next steps

That's all for this README! Jump to the API docs at <https://api.onyxframework.org/sql> or explore the [Crystal World](https://github.com/vladfaust/crystalworld) application built with Onyx, which is greatly documented as well!

Direct API links:

* [`Onyx::SQL::Model`](https://api.onyxframework.org/sql/Onyx/SQL/Model.html)
* [`Onyx::SQL::Query`](https://api.onyxframework.org/sql/Onyx/SQL/Query.html)
* [`Onyx::SQL::Repository`](https://api.onyxframework.org/sql/Onyx/SQL/Repository.html)

## Community 🍪

There are multiple places to talk about this particular shard and about other ones as well:

* [Onyx::SQL Gitter chat](https://gitter.im/onyxframework/sql)
* [Onyx Framework Gitter community](https://gitter.im/onyxframework)
* [Vlad Faust Gitter community](https://gitter.im/vladfaust)
* [Onyx Framework Twitter](https://twitter.com/onyxframework)
* [Onyx Framework Telegram channel](https://telegram.me/onyxframework)

## Support ❤️

This shard is maintained by me, [Vlad Faust](https://vladfaust.com), a passionate developer with years of programming and product experience. I love creating Open-Source and I want to be able to work full-time on Open-Source projects.

I will do my best to answer your questions in the free communication channels above, but if you want prioritized support, then please consider becoming my patron. Your issues will be labeled with your patronage status, and if you have a sponsor tier, then you and your team be able to communicate with me in private or semi-private channels such as e-mail and [Twist](https://twist.com). There are other perks to consider, so please, don't hesistate to check my Patreon page:

<a href="https://www.patreon.com/vladfaust"><img height="50" src="https://onyxframework.org/img/patreon-button.svg"></a>

You could also help me a lot if you leave a star to this GitHub repository and spread the world about Crystal and Onyx! 📣

## Contributing

1. Fork it ( https://github.com/onyxframework/http/fork )
2. Create your feature branch (git checkout -b my-new-feature)
3. Commit your changes (git commit -am 'feat: some feature') using [Angular style commits](https://github.com/angular/angular/blob/master/CONTRIBUTING.md#commit)
4. Push to the branch (git push origin my-new-feature)
5. Create a new Pull Request

## Contributors

- [Vlad Faust](https://github.com/vladfaust) - creator and maintainer

## Licensing

This software is licensed under [MIT License](LICENSE).

[![Open Source Initiative](https://upload.wikimedia.org/wikipedia/commons/thumb/4/42/Opensource.svg/100px-Opensource.svg.png)](https://opensource.org/licenses/MIT)
