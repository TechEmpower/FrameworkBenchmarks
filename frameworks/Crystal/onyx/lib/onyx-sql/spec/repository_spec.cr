require "./models"

alias Repository = Onyx::SQL::Repository

class MockDB
  record ResultSet

  class Driver
  end

  getter driver = Driver.new

  def initialize
  end

  getter latest_exec_sql : String? = nil
  getter latest_exec_params : Enumerable(DB::Any)? = nil

  def exec(sql, *params : DB::Any)
    @latest_exec_sql = sql
    @latest_exec_params = params.to_a
    return DB::ExecResult.new(0, 0)
  end

  def exec(sql, params : Enumerable(DB::Any)? = nil)
    @latest_exec_sql = sql
    @latest_exec_params = params.try &.to_a
    return DB::ExecResult.new(0, 0)
  end

  getter latest_scalar_sql : String? = nil
  getter latest_scalar_params : Enumerable(DB::Any)? = nil

  def scalar(sql : String, *params)
    @latest_scalar_sql = sql
    @latest_scalar_params = params.to_a
  end

  def scalar(sql : String, params : Enumerable(DB::Any)? = nil)
    @latest_scalar_sql = sql
    @latest_scalar_params = params.try &.to_a
    nil.as(DB::Any)
  end

  getter latest_query_sql : String? = nil
  getter latest_query_params : Enumerable(DB::Any)? = nil

  def query(sql, *params : DB::Any)
    @latest_query_sql = sql
    @latest_query_params = params.to_a
    ResultSet.new
  end

  def query(sql, params : Enumerable(DB::Any)? = nil)
    @latest_query_sql = sql
    @latest_query_params = params.try &.to_a
    ResultSet.new
  end
end

module Onyx::SQL
  class Repository
    def initialize(@db : MockDB, @logger = Onyx::SQL::Repository::Logger::Dummy.new)
    end
  end
end

class User
  def self.from_rs(rs : MockDB::ResultSet)
    [self.new(name: "Jake")]
  end
end
