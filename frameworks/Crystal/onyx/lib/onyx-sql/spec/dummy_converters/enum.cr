module DummyConverters
  module Enum(T)
    def self.to_db(value : T)
      value.to_s
    end

    def self.to_db(values : Enumerable(T))
      values.map(&.to_s).join(',')
    end

    def self.from_rs(rs : MockDB::ResultSet)
      rs.read(String | Nil).try { |s| T.parse(s) }
    end

    def self.from_rs_array(rs : MockDB::ResultSet)
      rs.read(String | Nil).try &.split(',').map { |s| T.parse(s) }
    end
  end
end
