import os

# from config.nims
putEnv("HOST", "0.0.0.0")
putEnv("DB_SQLITE", $true) # "true" or "false"
putEnv("DB_POSTGRES", $true) # "true" or "false"
putEnv("SESSION_TYPE", "file") # "file" or "redis"
putEnv("LIBSASS", $false) # "true" or "false"

# from .env
# Secret
putEnv("SECRET_KEY", "oaOPqiqLp7hNAN0izQl55p3SKAx9OM5Eob7DSmXDK5dRZj6QveHJPW8SRG3gbtwo7d4YeD9cbHQ64MuSK8tMLh00pNGoJw6O7nn8")

# DB Connection
putEnv("DB_DATABASE", "database")
putEnv("DB_USER", "user")
putEnv("DB_PASSWORD", "pass")
putEnv("DB_HOST", "postgreDb")
putEnv("DB_PORT", "5432") # postgres default...5432, mysql default...3306
putEnv("DB_MAX_CONNECTION", "498") # should be smaller than (DB max connection / running num processes)
putEnv("DB_TIMEOUT", "20") # secounds

# Logging
putEnv("LOG_IS_DISPLAY", $false) # true or false
putEnv("LOG_IS_FILE", $false) # true or false
putEnv("LOG_IS_ERROR_FILE", $false) # true or false
putEnv("LOG_DIR", "./logs")

# Session db
# Session type, file or redis, is defined in config.nims
putEnv("SESSION_DB_PATH", "./session.db") # Session file path or redis host:port. ex:"127.0.0.1:6379"
putEnv("SESSION_TIME", "20160") # minutes of 2 weeks
putEnv("COOKIE_DOMAINS", "") # to specify multiple domains, "sample.com, sample.org"

putEnv("LOCALE", "en")
