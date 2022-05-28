library(plumber)
library(dplyr)
library(DBI)


READ_ROW_SQL_BASE = 'SELECT "randomnumber", "id" FROM "world" WHERE id = '
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'

db_con <- dbConnect(RPostgres::Postgres(), dbname = "hello_world", host="tfb-database", port=5432, user="benchmarkdbuser", password="benchmarkdbpass")


get_num_queries <- function(queries){
  query_count <- 1
  query_count <- as.numeric(queries)
  if(is.na(query_count)) query_count <- 1
  if(query_count < 1) return(1)
  if(query_count > 500) return(500)
  return(query_count)
}


#* @get /query
#* @param queries
#* @serializer json
function(req, res, queries = NULL) {
  res$headers$Server <- "example"
  if(is.null(queries)) queries <- 1
  num_queries = get_num_queries(queries)
  row_ids = sample.int(10000, num_queries)

  output_list <- list()
  for(row_id in row_ids){
    # number = dbFetch(dbSendQuery(db_con, paste0(READ_ROW_SQL_BASE, row_id)))
    number = dbGetQuery(db_con, paste0(READ_ROW_SQL_BASE, row_id))

    output_list <- c(output_list, list(list('id' = row_id, 'randomNumber'= number$randomnumber)))
  }
  # print(jsonlite::toJSON(plyr::ldply(output_list, as.data.frame)))
  plyr::ldply(output_list, as.data.frame)
}


#* @get /fortunes
#* @serializer html
function(req, res) {
  res$headers$Server <- "example"

  fortunes_result <- dbGetQuery(db_con, 'SELECT * FROM Fortune')
  # fortunes_result <- c(fortunes_result, list(id = 0, message = 'Additional fortune added at request time.'))
  # fortunes_result[[1]] <- c(fortunes_result[[1]], 0)
  # fortunes_result[[2]] <- c(fortunes_result[[2]], 'Additional fortune added at request time.')
  # print(typeof(fortunes_result))
  # print(length(fortunes_result))
  # print(fortunes_result[[1]])
  # print(fortunes_result[[2]])
  # print(length(fortunes_result[[2]]))
  # print("hello")
  fortunes_df <- as.data.frame(fortunes_result)
  print(nrow(fortunes_df))
  fortunes_df <- rbind(fortunes_df, data.frame(id = 0, message = 'Additional fortune added at request time.'))
  print(fortunes_df$message[12])
  fortunes_df <- fortunes_df[order(fortunes_df$message), ]
  print("data frame")
  print(colnames(fortunes_df))
  # as.data.frame(fortunes_result)
  output_string <- "<!doctype html>
  <html>
  <head>
  <title>Fortunes</title>
  </head>
  <body>
  <table>
  <tr><th>id</th><th>message</th></tr>"

  for(i in 1:nrow(fortunes_df)){
    output_string <- paste0(output_string, paste0("<tr><td>", fortunes_df[i, 'id'],
     "</td><td>", fortunes_df[i, 'message'] %>% stringr::str_replace_all("<","&lt;") %>% stringr::str_replace_all(">","&gt;"), "</td></tr>"))
  }
  paste0(output_string , "</table>
  </body>
  </html>")
}


#* @get /json
#* @serializer unboxedJSON
function(req, res) {
  res$headers$Server <- "example"
  list('message'= 'Hello, world!')
}

#* @get /plaintext
#* @serializer text
function(req, res) {
  db_con <- dbConnect(RPostgres::Postgres(), dbname = "hello_world", host="tfb-database", port=5432, user="benchmarkdbuser", password="benchmarkdbpass")
  res$headers$Server <- "example"
  'Hello, World!'
}


#* @get /db
#* @serializer unboxedJSON
function(req, res) {
  res$headers$Server <- "example"
  row_id = sample.int(10000, 1)

  # number = dbFetch(dbSendQuery(db_con, paste0(READ_ROW_SQL_BASE, row_id)))
  number = dbGetQuery(db_con, paste0(READ_ROW_SQL_BASE, row_id))

  list('id' = row_id, 'randomNumber'= number$randomnumber)
}
