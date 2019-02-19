module DummyConverters
  module Int32Array
    def self.to_db(value : ::Array(Int32))
      value.map(&.to_s).join(',')
    end

    def self.from_rs(rs : MockDB::ResultSet)
      rs.read(String | Nil).try &.split(',').map { |s| s.to_i }
    end
  end
end
