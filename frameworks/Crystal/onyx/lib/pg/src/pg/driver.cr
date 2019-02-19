class PG::Driver < ::DB::Driver
  def build_connection(context : ::DB::ConnectionContext)
    Connection.new(context)
  end
end

DB.register_driver "postgres", PG::Driver
DB.register_driver "postgresql", PG::Driver
