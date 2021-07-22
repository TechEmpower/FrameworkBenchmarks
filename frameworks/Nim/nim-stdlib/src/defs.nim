import db_postgres

let db* = open("tfb-database:5432", "benchmarkdbuser", "benchmarkdbpass", "hello_world")
