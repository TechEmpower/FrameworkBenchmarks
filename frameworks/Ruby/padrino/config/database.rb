##
# A MySQL connection:
# DataMapper.setup(:default, 'mysql://user:password@localhost/the_database_name')
#
# # A Postgres connection:
# DataMapper.setup(:default, 'postgres://user:password@localhost/the_database_name')
#
# # A Sqlite3 connection
# DataMapper.setup(:default, "sqlite3://" + Padrino.root('db', "development.db"))
#

DataMapper.logger = logger
DataMapper::Property::String.length(255)

host = ENV['DB_HOST'] || 'localhost'
case Padrino.env
  when :production  then DataMapper.setup(:default, "mysql://benchmarkdbuser:benchmarkdbpass@#{host}/hello_world")
end
