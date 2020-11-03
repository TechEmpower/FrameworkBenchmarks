class AppDatabase < Avram::Database
  @@lock = Mutex.new

  # added to avram in https://github.com/luckyframework/avram/commit/419cca8d25512be45766d3224e5c56deb2ce5bd2
  private def db : DB::Database
    @@db ||= @@lock.synchronize do
      # check @@db again because a previous request could have set it after
      # the first time it was checked
      @@db || Avram::Connection.new(url, database_class: self.class).open
    end
  end
end
