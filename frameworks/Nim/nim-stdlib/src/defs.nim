import db_postgres

let db* = open("tfb-database:5432", "benchmarkdbuser", "benchmarkdbpass", "hello_world")

type
    DBQueryObj* = ref object of RootObj
        id*: string
        randomNumber*: string
    
    DBQueryObjsList* = seq[DBQueryObj]
    
