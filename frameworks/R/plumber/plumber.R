library(plumber)
library(DBI)


READ_ROW_SQL_BASE = 'SELECT "randomnumber", "id" FROM "world" WHERE id = '
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'

db_con <- dbConnect(RPostgres::Postgres(), dbname = "hello_world", host="tfb-database", port=5432, user="benchmarkdbuser", password="benchmarkdbpass")


#* @get /json
#* @serializer unboxedJSON
function(req, res) {
  res$headers$Server <- "example"
  list('message'= 'Hello, world!')
}

#* @get /plaintext
#* @serializer text
function(req, res) {
  res$headers$Server <- "example"
  'Hello, World!'
}


#* @get /db
#* @serializer unboxedJSON
function(req, res) {
  res$headers$Server <- "example"
  row_id = sample.int(10000, 1)

  number = dbFetch(dbSendQuery(db_con, paste0(READ_ROW_SQL_BASE, row_id)))

  list('id' = row_id, 'randomNumber'= number$randomnumber)
}
