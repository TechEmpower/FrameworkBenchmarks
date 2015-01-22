class World
  def db
    @db ||= Userdata.new("my_#{Process.pid}").db
  end

  def find(id)
    ret = nil
    db.execute("select * from World where id = ?", id) do |row, fields|
      ret = Hash[fields.zip(row)]
    end
    ret
  end

  def save(world)
    db.execute("update World set randomNumber = ? where id = ?", world["randomNumber"], world["randomNumber"])
  end
end

r = Nginx::Request.new

num_queries = r.var.arg_queries.to_i
num_queries = 1 if num_queries < 1
num_queries = 500 if num_queries > 500

world = World.new
ret = num_queries.times.map { world.find(rand(10000)) }
Nginx.rputs JSON::stringify(ret)
