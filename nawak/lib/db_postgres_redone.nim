#
#
#            Nimrod's Runtime Library
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## A higher level `PostgreSQL`:idx: database wrapper. This interface 
## is implemented for other databases too.

import strutils, postgres_redone

type
  TDbConn* = PPGconn   ## encapsulates a database connection
  TRow* = seq[string]  ## a row of a dataset. NULL database values will be
                       ## transformed always to the empty string.
  EDb* = object of EIO ## exception that is raised if a database error occurs
  
  TSqlQuery* = distinct string ## an SQL query string
  TPreparedId* = distinct string ## a identifier for the prepared queries

  FDb* = object of FIO ## effect that denotes a database operation
  FReadDb* = object of FDB   ## effect that denotes a read operation
  FWriteDb* = object of FDB  ## effect that denotes a write operation
  
proc sql*(query: string): TSqlQuery {.noSideEffect, inline.} =  
  ## constructs a TSqlQuery from the string `query`. This is supposed to be 
  ## used as a raw-string-literal modifier:
  ## ``sql"update user set counter = counter + 1"``
  ##
  ## If assertions are turned off, it does nothing. If assertions are turned 
  ## on, later versions will check the string for valid syntax.
  result = TSqlQuery(query)
 
proc dbError*(db: TDbConn) {.noreturn.} = 
  ## raises an EDb exception.
  var e: ref EDb
  new(e)
  e.msg = $PQerrorMessage(db)
  raise e

proc dbError*(msg: string) {.noreturn.} = 
  ## raises an EDb exception with message `msg`.
  var e: ref EDb
  new(e)
  e.msg = msg
  raise e

proc dbQuote(s: string): string =
  result = "'"
  for c in items(s):
    if c == '\'': add(result, "''")
    else: add(result, c)
  add(result, '\'')

proc dbFormat(formatstr: TSqlQuery, args: varargs[string]): string =
  result = ""
  var a = 0
  for c in items(string(formatstr)):
    if c == '?':
      add(result, dbQuote(args[a]))
      inc(a)
    else: 
      add(result, c)
  
proc TryExec*(db: TDbConn, query: TSqlQuery, 
              args: varargs[string, `$`]): bool {.tags: [FReadDB, FWriteDb].} =
  ## tries to execute the query and returns true if successful, false otherwise.
  var q = dbFormat(query, args)
  var res = PQExec(db, q)
  result = PQresultStatus(res) == PGRES_COMMAND_OK
  PQclear(res)

proc ExecNo*(db: TDbConn, query: TSqlQuery, args: varargs[string, `$`]) {.
  tags: [FReadDB, FWriteDb].} =
  ## executes the query and raises EDB if not successful.
  var q = dbFormat(query, args)
  var res = PQExec(db, q)
  if PQresultStatus(res) != PGRES_COMMAND_OK: dbError(db)
  PQclear(res)

var nullPOid: POid
var nowhere: ptr int32
proc Exec*(db: TDbConn, stmtName: TPreparedId,
          args: varargs[string]) {.tags: [FReadDB, FWriteDb].} =
    var arr = allocCStringArray(args)

    var res = PQexecPrepared(db, stmtName.string, int32(args.len), arr,
                            nowhere, nowhere, 0)

    deallocCStringArray(arr)
    if PQResultStatus(res) != PGRES_COMMAND_OK: dbError(db)
    PQclear(res)

  
proc newRow(L: int): TRow =
  newSeq(result, L)
  for i in 0..L-1: result[i] = ""
  
proc setupQueryNo(db: TDbConn, query: TSqlQuery, 
                args: varargs[string]): PPGresult = 
  var q = dbFormat(query, args)
  result = PQExec(db, q)
  if PQresultStatus(result) != PGRES_TUPLES_OK: dbError(db)


proc setupQuery(db: TDbConn, query: string,
                args: varargs[string]): PPGresult =
    # read this for details:
    # http://www.postgresql.org/docs/9.3/interactive/libpq-exec.html
    var arr = allocCStringArray(args)

    result = PQexecParams(db, query, int32(args.len), nullPOid, arr, nowhere, nowhere, 0)

    deallocCStringArray(arr)
    if PQResultStatus(result) != PGRES_TUPLES_OK: dbError(db)

proc setupPQuery(db: TDbConn, stmtName: TPreparedId,
                args: varargs[string]): PPGresult =
    var arr = allocCStringArray(args)

    result = PQexecPrepared(db, stmtName.string, int32(args.len), arr,
                            nowhere, nowhere, 0)

    deallocCStringArray(arr)
    if PQResultStatus(result) != PGRES_TUPLES_OK: dbError(db)

proc prepare*(db: TDbConn; stmtName, query: string; nParams: int): TPreparedId =
    var res = PQprepare(db, stmtName, query, int32(nParams), nullPOid)
    if PQResultStatus(res) != PGRES_COMMAND_OK: dbError(db)
    return TPreparedId(stmtName)
   
proc setRow(res: PPGresult, r: var TRow, line, cols: int32) =
  for col in 0..cols-1:
    setLen(r[col], 0)
    var x = PQgetvalue(res, line, col)
    add(r[col], x)

iterator FastPRows*(db: TDbConn, stmtName: TPreparedId,
                   args: varargs[string, `$`]): TRow {.tags: [FReadDB].} =
  ## executes the query and iterates over the result dataset. This is very 
  ## fast, but potenially dangerous: If the for-loop-body executes another
  ## query, the results can be undefined. For Postgres it is safe though.
  var res = setupPQuery(db, stmtName, args)
  var L = PQnfields(res)
  var result = newRow(L)
  for i in 0..PQntuples(res)-1:
    setRow(res, result, i, L)
    yield result
  PQclear(res)

iterator FastRows*(db: TDbConn, query: string,
                   args: varargs[string, `$`]): TRow {.tags: [FReadDB].} =
  ## executes the query and iterates over the result dataset. This is very 
  ## fast, but potenially dangerous: If the for-loop-body executes another
  ## query, the results can be undefined. For Postgres it is safe though.
  var res = setupQuery(db, query, args)
  var L = PQnfields(res)
  var result = newRow(L)
  for i in 0..PQntuples(res)-1:
    setRow(res, result, i, L)
    yield result
  PQclear(res)

proc getPRow*(db: TDbConn, stmtName: TPreparedId,
              args: varargs[string, `$`]): TRow {.tags: [FReadDB].} =
    var res = setupPQuery(db, stmtName, args)
    var L = PQnfields(res)
    result = newRow(L)
    setRow(res, result, 0, L)
    PQclear(res)

proc getRow*(db: TDbConn, query: string,
             args: varargs[string, `$`]): TRow {.tags: [FReadDB].} =
  ## retrieves a single row. If the query doesn't return any rows, this proc
  ## will return a TRow with empty strings for each column.
  var res = setupQuery(db, query, args)
  var L = PQnfields(res)
  result = newRow(L)
  setRow(res, result, 0, L)
  PQclear(res)

proc GetAllPRows*(db: TDbConn, stmtName: TPreparedId,
                 args: varargs[string, `$`]): seq[TRow] {.tags: [FReadDB].} =
  ## executes the query and returns the whole result dataset.
  result = @[]
  for r in FastPRows(db, stmtName, args):
    result.add(r)

proc GetAllRows*(db: TDbConn, query: string, 
                 args: varargs[string, `$`]): seq[TRow] {.tags: [FReadDB].} =
  ## executes the query and returns the whole result dataset.
  result = @[]
  for r in FastRows(db, query, args):
    result.add(r)

iterator Rows*(db: TDbConn, query: string, 
               args: varargs[string, `$`]): TRow {.tags: [FReadDB].} =
  ## same as `FastRows`, but slower and safe.
  for r in items(GetAllRows(db, query, args)): yield r

proc GetValue*(db: TDbConn, query: string, 
               args: varargs[string, `$`]): string {.tags: [FReadDB].} = 
  ## executes the query and returns the first column of the first row of the
  ## result dataset. Returns "" if the dataset contains no rows or the database
  ## value is NULL.
  var x = PQgetvalue(setupQuery(db, query, args), 0, 0)
  result = if isNil(x): "" else: $x
  
proc TryInsertID*(db: TDbConn, query: string, 
                  args: varargs[string, `$`]): int64  {.tags: [FWriteDb].}=
  ## executes the query (typically "INSERT") and returns the 
  ## generated ID for the row or -1 in case of an error. For Postgre this adds
  ## ``RETURNING id`` to the query, so it only works if your primary key is
  ## named ``id``. 
  var x = PQgetvalue(setupQuery(db, query & " RETURNING id", 
    args), 0, 0)
  if not isNil(x):
    result = ParseBiggestInt($x)
  else:
    result = -1

proc InsertID*(db: TDbConn, query: string, 
               args: varargs[string, `$`]): int64 {.tags: [FWriteDb].} =
  ## executes the query (typically "INSERT") and returns the 
  ## generated ID for the row. For Postgre this adds
  ## ``RETURNING id`` to the query, so it only works if your primary key is
  ## named ``id``. 
  result = TryInsertID(db, query, args)
  if result < 0: dbError(db)
  
proc ExecAffectedRows*(db: TDbConn, query: TSqlQuery, 
                       args: varargs[string, `$`]): int64 {.tags: [
                       FReadDB, FWriteDb].} = 
  ## executes the query (typically "UPDATE") and returns the
  ## number of affected rows.
  var q = dbFormat(query, args)
  var res = PQExec(db, q)
  if PQresultStatus(res) != PGRES_COMMAND_OK: dbError(db)
  result = parseBiggestInt($PQcmdTuples(res))
  PQclear(res)

proc Close*(db: TDbConn) {.tags: [FDb].} = 
  ## closes the database connection.
  if db != nil: PQfinish(db)

proc Open*(connection, user, password, database: string): TDbConn {.
  tags: [FDb].} =
  ## opens a database connection. Raises `EDb` if the connection could not
  ## be established.
  ##
  ## Clients can also use Postgres keyword/value connection strings to
  ## connect.
  ##
  ## Example:
  ##
  ## .. code-block:: nimrod
  ##
  ##      con = Open("", "", "", "host=localhost port=5432 dbname=mydb")
  ##
  ## See http://www.postgresql.org/docs/current/static/libpq-connect.html#LIBPQ-CONNSTRING
  ## for more information.
  ##
  ## Note that the connection parameter is not used but exists to maintain
  ## the nimrod db api.
  result = PQsetdbLogin(nil, nil, nil, nil, database, user, password)
  if PQStatus(result) != CONNECTION_OK: dbError(result) # result = nil


