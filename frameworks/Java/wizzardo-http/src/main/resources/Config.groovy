server {
    name = 'W'
    host = '0.0.0.0'
    port = 8080
    ttl = 5 * 60 * 1000
    context = '/'

    session {
        ttl = 30 * 60
    }

    debugOutput = false
    onlyCachedHeaders = true
}

db {
    dbname = 'hello_world'
    host = 'tfb-database'
    port = 5432

    username = 'benchmarkdbuser'
    password = 'benchmarkdbpass'

    maximumPoolSize = 128
    minimumIdle = 128
}