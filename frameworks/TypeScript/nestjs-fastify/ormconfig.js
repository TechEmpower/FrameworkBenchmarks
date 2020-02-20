let config = {
    "synchronize": false,
    "logging": false,
    "entities": ["dist/**/*.entity{.ts,.js}"],
}

config = {
    ...config,
    "type": "postgres",
    "host": "tfb-database",
    "port": 5432,
    "username": "benchmarkdbuser",
    "password": "benchmarkdbpass",
    "database": "hello_world",
}

module.exports = config
