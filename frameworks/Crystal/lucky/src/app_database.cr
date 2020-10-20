class AppDatabase < Avram::Database
  @@lock = Mutex.new

  private def db
    @@db ||= @@lock.synchronize { Avram::Connection.new(url, database_class: self.class).open }
  end
end
