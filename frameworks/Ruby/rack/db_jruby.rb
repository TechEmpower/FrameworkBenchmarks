# frozen_string_literal: true

require 'sequel'

def connect(url)
  max_connections = ENV['MAX_THREADS'].to_i
  JDBCConnection.new("jdbc:" + url, max_connections: max_connections)
end

class JDBCConnection
  attr_reader :connection

  def initialize(connection_string, max_connections:)
    @connection = Sequel.connect(connection_string, max_connections: max_connections)
    @world_select = @connection['SELECT id, randomNumber FROM World WHERE id = ?', :$id].prepare(:select, :select_by_id)
    @fortune_select = @connection['SELECT id, message FROM Fortune'].prepare(:select, :select_all)
  end

  def select_world(id)
    @world_select.call(id: id).first.transform_keys(&:to_s)
  end

  def select_fortunes
    @fortune_select.call.map { it.transform_keys(&:to_s) }
  end

  def exec(query)
    @connection[query].update
  end

  def with
    yield(self)
  end
end
