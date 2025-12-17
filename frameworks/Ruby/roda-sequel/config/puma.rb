before_fork do
  Sequel::DATABASES.each(&:disconnect)
end
