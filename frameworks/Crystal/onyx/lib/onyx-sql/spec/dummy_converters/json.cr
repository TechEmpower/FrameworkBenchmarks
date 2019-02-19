module DummyConverters
  module JSON(T)
    def self.to_db(value : T)
      value.to_json
    end

    def self.from_rs(rs : MockDB::ResultSet)
      T.from_json(rs.read(String))
    end
  end
end
