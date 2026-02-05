# frozen_string_literal: true

require 'pg'
require 'connection_pool'

def connect(url)
  if defined?(Puma) || defined?(Itsi)
    max_connections = ENV.fetch('MAX_THREADS')
  elsif defined?(Iodine)
    max_connections = ENV.fetch('THREADS')
  else
    max_connections = 512
  end
  ConnectionPool.new(size: max_connections, timeout: 5) do
    PGConnection.new(url)
  end
end

class PGConnection
  attr_reader :connection

  def initialize(connection_string)
    @connection = PG::Connection.new(connection_string)
    @connection.set_error_verbosity(PG::PQERRORS_VERBOSE)
    @connection.prepare('select_world', 'SELECT id, randomNumber FROM world WHERE id = $1')
  end

  def select_world(id)
    @connection.exec_prepared('select_world', [id]).first
  end

  def select_fortunes
    @connection.exec('SELECT id, message FROM fortune')
  end

  def exec(query)
    @connection.exec(query)
  end
end
