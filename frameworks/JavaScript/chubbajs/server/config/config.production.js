module.exports = {
  port: 8080,
  projectRoot: "/build",
  database: {
    engine: "postgres",
    host: "TFB-database",
    user: "benchmarkdbuser",
    password: "benchmarkdbpass",
    database: "hello_world",
    port: 5432,
    // Will check models against current scheme and perform migrations
    migrations: false
  }
};
