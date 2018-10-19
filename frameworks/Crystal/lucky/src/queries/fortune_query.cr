class FortuneQuery < Fortune::BaseQuery
  def fortunes
    data = Array(NamedTuple(id: Int32, message: String)).new
    LuckyRecord::Repo.run do |db|
      db.query_each("SELECT id, message FROM Fortune") do |rs|
        data.push({id: rs.read(Int32), message: rs.read(String)})
      end
    end
    data
  end

end
