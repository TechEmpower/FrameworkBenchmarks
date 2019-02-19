[![Build Status](https://travis-ci.org/crystal-lang/crystal-db.svg?branch=master)](https://travis-ci.org/crystal-lang/crystal-db)

# crystal-db

Common db api for crystal. You will need to have a specific driver to access a database.

* [sqlite](https://github.com/crystal-lang/crystal-sqlite3)
* [mysql](https://github.com/crystal-lang/crystal-mysql)
* [postgres](https://github.com/will/crystal-pg)

## Installation

Add this to your application's `shard.yml`:

```yaml
dependencies:
  db:
    github: crystal-lang/crystal-db
```

## Documentation

* [Latest API](http://crystal-lang.github.io/crystal-db/api/latest/)
* [Crystal book](https://crystal-lang.org/docs/database/)

## Usage

Since this is an abstract db api, it's usage is through a concrete database driver.

```crystal
require "db"
require "sqlite3"

DB.open "sqlite3:./file.db" do |db|
  # When using the pg driver, use $1, $2, etc. instead of ?
  db.exec "create table contacts (name text, age integer)"
  db.exec "insert into contacts values (?, ?)", "John Doe", 30

  args = [] of DB::Any
  args << "Sarah"
  args << 33
  db.exec "insert into contacts values (?, ?)", args

  puts "max age:"
  puts db.scalar "select max(age) from contacts" # => 33

  puts "contacts:"
  db.query "select name, age from contacts order by age desc" do |rs|
    puts "#{rs.column_name(0)} (#{rs.column_name(1)})"
    # => name (age)
    rs.each do
      puts "#{rs.read(String)} (#{rs.read(Int32)})"
      # => Sarah (33)
      # => John Doe (30)
    end
  end
end
```

## Roadmap

Issues not yet addressed

- [x] Support non prepared statements. [#25](https://github.com/crystal-lang/crystal-db/pull/25)
- [x] Time data type. (implementation details depends on actual drivers)
- [x] Data type extensibility. Allow each driver to extend the data types allowed.
- [x] Transactions & nested transactions. [#27](https://github.com/crystal-lang/crystal-db/pull/27)
- [x] Connection pool.
- [ ] Direct access to `IO` to avoid memory allocation for blobs.

## Contributing

1. Fork it ( https://github.com/crystal-lang/crystal-db/fork )
2. Create your feature branch (git checkout -b my-new-feature)
3. Commit your changes (git commit -am 'Add some feature')
4. Push to the branch (git push origin my-new-feature)
5. Create a new Pull Request

## Contributors

- [bcardiff](https://github.com/bcardiff) Brian J. Cardiff - creator, maintainer
