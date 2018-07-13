database = "lucky_#{Lucky::Env.name}"

LuckyRecord::Repo.configure do
  if Lucky::Env.production?
    settings.url = ENV.fetch("DATABASE_URL")
  else
    settings.url = ENV["DATABASE_URL"]? || LuckyRecord::PostgresURL.build(
      hostname: "localhost",
      database: database
    )
  end
  # In development and test, raise an error if you forget to preload associations
  settings.lazy_load_enabled = Lucky::Env.production?
end

LuckyMigrator::Runner.configure do
  settings.database = database
end
