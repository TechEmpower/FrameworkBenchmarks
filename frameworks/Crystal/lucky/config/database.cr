AppDatabase.configure do |settings|
  settings.credentials = Avram::Credentials.parse(ENV["DATABASE_URL"])
end

Avram.configure do |settings|
  settings.database_to_migrate = AppDatabase

  # In production, allow lazy loading (N+1).
  # In development and test, raise an error if you forget to preload associations
  settings.lazy_load_enabled = false
end
