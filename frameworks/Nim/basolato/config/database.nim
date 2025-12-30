import std/os
import std/strutils
import db_connector/db_postgres
import allographer/connection

when defined(release):
  import std/cpuinfo

let maxConnections =
  when defined(release):
    (getEnv("DB_MAX_CONNECTION").parseInt div countProcessors()) - 2
  else:
    95
echo "maxConnections: ",maxConnections

let rdb* = dbopen(
  PostgreSQL, # SQLite3 or MySQL or MariaDB or PostgreSQL
  getEnv("DB_DATABASE"),
  getEnv("DB_USER"),
  getEnv("DB_PASSWORD"),
  getEnv("DB_HOST"),
  getEnv("DB_PORT").parseInt,
  maxConnections,
  getEnv("DB_TIMEOUT").parseInt,
  getEnv("LOG_IS_DISPLAY").parseBool,
  getEnv("LOG_IS_FILE").parseBool,
  getEnv("LOG_DIR"),
)

let stdRdb* = open(getEnv("DB_HOST"), getEnv("DB_USER"), getEnv("DB_PASSWORD"), getEnv("DB_DATABASE"))
