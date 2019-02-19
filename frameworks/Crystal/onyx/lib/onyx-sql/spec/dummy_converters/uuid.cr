module DummyConverters
  module UUID
    def self.to_db(value : ::UUID)
      value.to_s
    end

    def self.to_db(values : Enumerable(::UUID))
      values.map(&.to_s).join(',')
    end

    def self.from_rs(rs : MockDB::ResultSet)
      ::UUID.new(rs.read(String))
    end
  end
end
