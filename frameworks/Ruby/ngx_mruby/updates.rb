r = Nginx::Request.new

num_queries = r.var.arg_queries.to_i
num_queries = 1 if num_queries < 0
num_queries = 500 if num_queries > 500

class Worlds
  def db
    @db ||= Userdata.new("my_#{Process.pid}").db
  end

  def find(id)
    db.execute("select * from World where id = ?", id) do |row, fields|
      return Hash[fields.zip(row)]
    end
  end

  def save(world)
    #TODO: consider execute_batch
    db.execute("update World set randomNumber = ? where id = ?", world["randomNumber"], world["randomNumber"])
  end
end

ret = num_queries.times.map { World.find(rand(10000)) }

ret.each do |world|
  world["randomNumber"] = rand(10000) + 1
  World.save(world)
end

Nginx.rputs JSON::stringify(ret)
