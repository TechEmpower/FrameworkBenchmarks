abstract class BaseModel < Avram::Model
  def self.database : Avram::Database.class
    AppDatabase
  end
end
