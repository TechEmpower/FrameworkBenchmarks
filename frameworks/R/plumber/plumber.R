library(plumber)


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
