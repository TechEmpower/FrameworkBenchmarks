require "dotenv"
require "./utils/require_env"

ENV["CRYSTAL_ENV"] = "development" unless ENV.has_key?("CRYSTAL_ENV")

Dotenv.verbose = false

Dotenv.load(".env")
Dotenv.load(".env.local")
Dotenv.load(".env.#{ENV["CRYSTAL_ENV"]}")
Dotenv.load(".env.#{ENV["CRYSTAL_ENV"]}.local")
