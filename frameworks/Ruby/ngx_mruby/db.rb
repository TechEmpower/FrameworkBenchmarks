# https://github.com/mattn/mruby-mysql/blob/master/example/example.rb
db = Userdata.new("my_#{Process.pid}").db

ret = nil
db.execute("select * from World where id = ?", rand(10000)) do |row, fields|
  ret = Hash[fields.zip(row)]
end
Nginx.rputs JSON::stringify(ret)
