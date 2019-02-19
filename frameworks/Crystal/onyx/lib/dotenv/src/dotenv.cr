require "./dotenv/*"

module Dotenv
  extend self

  class FileMissing < Exception
  end

  @@verbose = true

  def verbose=(value : Bool) : Bool
    @@verbose = value
  end

  def load(path = ".env") : Hash(String, String)
    load File.open(File.expand_path(path))
  rescue ex
    log "DOTENV - Could not open file: #{path}"
    {} of String => String
  end

  def load(io : IO) : Hash(String, String)
    hash = {} of String => String
    io.each_line do |line|
      handle_line line, hash
    end
    load hash
    hash
  end

  def load(hash : Hash(String, String))
    hash.each do |key, value|
      ENV[key] = value
    end
    ENV
  end

  def load!(path = ".env") : Hash(String, String)
    load File.open(File.expand_path(path))
  rescue ex
    raise FileMissing.new("Missing file!")
  end

  def load!(io : IO) : Hash(String, String)
    load(io)
  end

  def load!(hash : Hash(String, String))
    load(hash)
  end

  private def handle_line(line, hash)
    if line !~ /\A\s*(?:#.*)?\z/m
      name, value = line.split("=", 2)
      hash[name.strip] = value.strip
    end
  rescue ex
    log "DOTENV - Malformed line #{line}"
  end

  private def log(message : String)
    puts message if @@verbose
  end
end
