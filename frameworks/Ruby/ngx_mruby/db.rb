# https://github.com/mattn/mruby-mysql/blob/master/example/example.rb
db = Userdata.new("my_#{Process.pid}").db
result_set = db.execute("select * from World where id = ?", rand(10000))
world = Hash[result_set.fields.zip(result_set.next)]
result_set.close
Nginx.rputs JSON::stringify(world)
