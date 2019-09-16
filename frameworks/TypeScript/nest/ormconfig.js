module.exports = {
    "type": "postgres",
    "host": "tfb-database",
    "port": 5432,
    "username": "benchmarkdbuser",
    "password": "benchmarkdbpass",
    "database": "hello_world",
    "synchronize":false,
    "logging": false,
    "entities": process.env.NODE_ENV === 'production' ? ["./dist/**/*.entity.js"] : ["./dist/**/*.entity.js", "./src/**/*.entity.ts"]
}