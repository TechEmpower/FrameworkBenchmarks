program raw;

{
 ----------------------------------------------------
   TechEmpower Framework Benchmarks implementation
   in modern pascal and the mORMot 2 framework
 ----------------------------------------------------
 https://github.com/TechEmpower/FrameworkBenchmarks/wiki
 command line optional syntax: run "raw -?"
}

{$I mormot.defines.inc}

{.$define WITH_LOGS}
// logging is fine for debugging, less for benchmarking ;)

uses
  {$I mormot.uses.inc} // include mormot.core.fpcx64mm or mormot.core.fpclibcmm
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.log,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.json,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.perf,
  mormot.core.mustache,
  mormot.orm.core,
  mormot.orm.base,
  mormot.orm.sql,
  mormot.db.core,
  mormot.db.raw.sqlite3,
  mormot.db.raw.sqlite3.static,
  mormot.db.sql,
  mormot.db.sql.postgres,
  mormot.rest.sqlite3,
  mormot.net.http,
  mormot.net.server,
  mormot.net.async;

type
  // data structures
  TMessageRec = packed record
    message: PUtf8Char;
  end;
  TWorldRec = packed record
    id: integer;
    randomNumber: integer;
  end;
  TWorlds = array of TWorldRec;
  TFortune = packed record
    id: PtrUInt;
    message: PUtf8Char;
  end;
  TFortunes = array of TFortune;

  // ORM definitions
  TOrmWorld = class(TOrm)
  protected
    fRandomNumber: integer;
  published
    property randomNumber: integer
      read fRandomNumber write fRandomNumber;
  end;
  TOrmCachedWorld = class(TOrmWorld);
  TOrmWorlds = array of TOrmWorld;
  TOrmFortune = class(TOrm)
  protected
    fMessage: RawUtf8;
  published
    property Message: RawUtf8
      read fMessage write fMessage;
  end;
  TOrmFortunes = array of TOrmFortune;

  // main server class
  TRawAsyncServer = class(TSynPersistent)
  protected
    fHttpServer: THttpAsyncServer;
    fModel: TOrmModel;
    fStore: TRestServerDB;
    fTemplate: TSynMustache;
    fCachedWorldsTable: POrmCacheTable;
    fRawCache: TOrmWorlds;
    fDbPool: TSqlDBPostgresConnectionProperties;
    procedure OnAsyncDb(Statement: TSqlDBPostgresAsyncStatement; Context: TObject);
    procedure OnAsyncFortunes(Statement: TSqlDBPostgresAsyncStatement; Context: TObject);
    // pipelined reading as used by /rawqueries and /rawupdates
    function GetRawRandomWorlds(cnt: PtrInt; out res: TWorlds): boolean;
    function ComputeRawFortunes(stmt: TSqlDBStatement; ctxt: THttpServerRequest): integer;
  public
    constructor Create(threadCount: integer; flags: THttpServerOptions;
      pin2Core: integer); reintroduce;
    destructor Destroy; override;
  published
    // all service URI are implemented by these published methods using RTTI
    function plaintext(ctxt: THttpServerRequest): cardinal;
    function json(ctxt: THttpServerRequest): cardinal;
    function db(ctxt: THttpServerRequest): cardinal;
    function queries(ctxt: THttpServerRequest): cardinal;
    function cached_queries(ctxt: THttpServerRequest): cardinal;
    function fortunes(ctxt: THttpServerRequest): cardinal;
    function updates(ctxt: THttpServerRequest): cardinal;
    function rawdb(ctxt: THttpServerRequest): cardinal;
    function rawqueries(ctxt: THttpServerRequest): cardinal;
    function rawcached(ctxt: THttpServerRequest): cardinal;
    function rawfortunes(ctxt: THttpServerRequest): cardinal;
    function rawupdates(ctxt: THttpServerRequest): cardinal;
    // asynchronous PostgreSQL pipelined DB access
    function asyncdb(ctxt: THttpServerRequest): cardinal;
    function asyncqueries(ctxt: THttpServerRequest): cardinal;
    function asyncfortunes(ctxt: THttpServerRequest): cardinal;
    function asyncupdates(ctxt: THttpServerRequest): cardinal;
  end;

{$I-}

const
  HELLO_WORLD: RawUtf8 = 'Hello, World!';
  TEXT_CONTENT_TYPE_NO_ENCODING: RawUtf8 = 'text/plain';

  WORLD_COUNT       = 10000;
  WORLD_READ_SQL    = 'select id,randomNumber from World where id=?';
  WORLD_UPDATE_SQLN = 'update World as t set randomNumber = v.r from ' +
    '(SELECT unnest(?::integer[]), unnest(?::integer[]) order by 1) as v(id, r)' +
    ' where t.id = v.id';
  FORTUNES_SQL      = 'select id,message from Fortune';

  FORTUNES_MESSAGE = 'Additional fortune added at request time.';
  FORTUNES_TPL     = '<!DOCTYPE html>' +
                     '<html>' +
                     '<head><title>Fortunes</title></head>' +
                     '<body>' +
                     '<table>' +
                     '<tr><th>id</th><th>message</th></tr>' +
                     '{{#.}}' +
                     '<tr><td>{{id}}</td><td>{{message}}</td></tr>' +
                     '{{/.}}' +
                     '</table>' +
                     '</body>' +
                     '</html>';


function ComputeRandomWorld(gen: PLecuyer): integer; inline;
begin
  result := gen^.Next(WORLD_COUNT) + 1;
end;

function GetQueriesParamValue(ctxt: THttpServerRequest;
  const search: RawUtf8 = 'QUERIES='): cardinal; inline;
begin
  if not ctxt.UrlParam(search, result) or
     (result = 0) then
    result := 1
  else if result > 500 then
    result := 500;
end;


{ TRawAsyncServer }

constructor TRawAsyncServer.Create(
  threadCount: integer; flags: THttpServerOptions; pin2Core: integer);
begin
  inherited Create;
  fDbPool := TSqlDBPostgresConnectionProperties.Create(
    'tfb-database:5432', 'hello_world', 'benchmarkdbuser', 'benchmarkdbpass');
  fDbPool.ArrayParamsAsBinary := true;
  // customize JSON serialization for TFB expectations
  TOrmWorld.OrmProps.Fields.JsonRenameProperties([
    'ID',           'id',
    'RandomNumber', 'randomNumber']);
  TOrmCachedWorld.OrmProps.Fields.JsonRenameProperties([
    'ID',           'id',
    'RandomNumber', 'randomNumber']);
  // setup the ORM data model
  fModel := TOrmModel.Create([TOrmWorld, TOrmFortune, TOrmCachedWorld]);
  OrmMapExternal(fModel, [TOrmWorld, TOrmFortune], fDbPool);
  // CachedWorld table doesn't exists in DB, but should as read in requirements.
  // Use world table as in other implementations.
  OrmMapExternal(fModel, TOrmCachedWorld, fDbPool, 'world');
  // setup the main ORM store
  fStore := TRestServerDB.Create(fModel, SQLITE_MEMORY_DATABASE_NAME);
  fStore.NoAjaxJson := true;
  fStore.Server.CreateMissingTables; // create SQlite3 virtual tables
  // pre-fill the ORM
  if fStore.Server.Cache.SetCache(TOrmCachedWorld) then
    fStore.Server.Cache.FillFromQuery(TOrmCachedWorld, '', []);
  fCachedWorldsTable := fStore.Orm.Cache.Table(TOrmCachedWorld);
  fStore.Orm.RetrieveListObjArray(fRawCache, TOrmCachedWorld, 'order by id', []);
  // initialize the mustache template for /fortunes
  fTemplate := TSynMustache.Parse(FORTUNES_TPL);
  // setup the HTTP server
  fHttpServer := THttpAsyncServer.Create(
    '8080', nil, nil, '', threadCount,
    5 * 60 * 1000,         // 5 minutes keep alive connections
    [hsoNoXPoweredHeader,  // not needed for a benchmark
     hsoHeadersInterning,  // reduce memory contention for /plaintext and /json
     hsoNoStats,           // disable low-level statistic counters
     //hsoThreadCpuAffinity, // worse scaling on multi-servers
     hsoThreadSmooting,    // seems a good option, even if not magical
     hsoEnablePipelining,  // as expected by /plaintext
     {$ifdef WITH_LOGS}
     hsoLogVerbose,
     {$endif WITH_LOGS}
     hsoIncludeDateHeader  // required by TFB General Test Requirements #5
    ] + flags);
  if pin2Core <> -1 then
    fHttpServer.Async.SetCpuAffinity(pin2Core);
  fHttpServer.HttpQueueLength := 10000; // needed e.g. from wrk/ab benchmarks
  fHttpServer.ServerName := 'M';
  // use default routing using RTTI on the TRawAsyncServer published methods
  fHttpServer.Route.RunMethods([urmGet], self);
  // wait for the server to be ready and raise exception e.g. on binding issue
  fHttpServer.WaitStarted;
end;

destructor TRawAsyncServer.Destroy;
begin
  fHttpServer.Free;
  fStore.Free;
  fModel.Free;
  fDBPool.Free;
  ObjArrayClear(fRawCache);
  inherited Destroy;
end;

// query DB world table for /rawqueries and /rawupdates endpoints

function TRawAsyncServer.GetRawRandomWorlds(cnt: PtrInt;
  out res: TWorlds): boolean;
var
  conn: TSqlDBConnection;
  stmt: ISqlDBStatement;
  pConn: TSqlDBPostgresConnection absolute conn;
  pStmt: TSqlDBPostgresStatement;
  gen: PLecuyer;
  i: PtrInt;
begin
  result := false;
  SetLength(res{%H-}, cnt);
  gen := Lecuyer;
  conn := fDbPool.ThreadSafeConnection;
  // specific code to use PostgresSQL pipelining mode
  // see test_nosync in
  // https://github.com/postgres/postgres/blob/master/src/test/modules/libpq_pipeline/libpq_pipeline.c
  stmt := conn.NewStatementPrepared(WORLD_READ_SQL, true, true);
  pConn.EnterPipelineMode;
  pStmt := TSqlDBPostgresStatement(stmt.Instance);
  for i := 0 to cnt - 1 do
  begin
    pStmt.Bind(1, ComputeRandomWorld(gen));
    pStmt.SendPipelinePrepared;
    pConn.PipelineSync;
  end;
  for i := 0 to cnt - 1 do
  begin
    pStmt.GetPipelineResult;
    if not stmt.Step then
      exit;
    res[i].id := pStmt.ColumnInt(0);
    res[i].randomNumber := pStmt.ColumnInt(1);
    pStmt.ReleaseRows;
    pConn.CheckPipelineSync;
  end;
  pConn.ExitPipelineMode;
  result := true;
end;

function FortuneCompareByMessage(const A, B): integer;
begin
  result := StrComp(pointer(TFortune(A).message), pointer(TFortune(B).message));
end;

function TRawAsyncServer.ComputeRawFortunes(
  stmt: TSqlDBStatement; ctxt: THttpServerRequest): integer;
var
  list: TFortunes;
  arr: TDynArray;
  n: integer;
  f: ^TFortune;
  mus: TSynMustacheContextData;
begin
  result := HTTP_BADREQUEST;
  if stmt = nil then
    exit;
  arr.Init(TypeInfo(TFortunes), list, @n);
  while stmt.Step do
  begin
    f := arr.NewPtr;
    f.id := stmt.ColumnInt(0);
    f.message := stmt.ColumnPUtf8(1);
  end;
  f := arr.NewPtr;
  f.id := 0;
  f.message := FORTUNES_MESSAGE;
  arr.Sort(FortuneCompareByMessage);
  mus := stmt.Connection.GetThreadOwned(TSynMustacheContextData);
  if mus = nil then
    mus := stmt.Connection.SetThreadOwned(fTemplate.NewMustacheContextData);
  ctxt.OutContent := mus.RenderArray(arr);
  ctxt.OutContentType := HTML_CONTENT_TYPE;
  result := HTTP_SUCCESS;
end;

// following methods implement the server endpoints

function TRawAsyncServer.plaintext(ctxt: THttpServerRequest): cardinal;
begin
  ctxt.OutContent := HELLO_WORLD;
  ctxt.OutContentType := TEXT_CONTENT_TYPE_NO_ENCODING;
  result := HTTP_SUCCESS;
end;

function TRawAsyncServer.json(ctxt: THttpServerRequest): cardinal;
var
  msgRec: TMessageRec;
begin
  msgRec.message := pointer(HELLO_WORLD);
  ctxt.SetOutJson(@msgRec, TypeInfo(TMessageRec));
  result := HTTP_SUCCESS;
end;

function TRawAsyncServer.db(ctxt: THttpServerRequest): cardinal;
var
  w: TOrmWorld;
begin
  w := TOrmWorld.Create(fStore.Orm, ComputeRandomWorld(Lecuyer));
  try
    ctxt.SetOutJson(w);
    result := HTTP_SUCCESS;
  finally
    w.Free;
  end;
end;

function TRawAsyncServer.queries(ctxt: THttpServerRequest): cardinal;
var
  i: PtrInt;
  res: TOrmWorlds;
  gen: PLecuyer;
begin
  SetLength(res, GetQueriesParamValue(ctxt, 'QUERIES='));
  gen := Lecuyer;
  for i := 0 to length(res) - 1 do
    res[i] := TOrmWorld.Create(fStore.Orm, ComputeRandomWorld(gen));
  ctxt.SetOutJson(@res, TypeInfo(TOrmWorlds));
  ObjArrayClear(res);
  result := HTTP_SUCCESS;
end;

function TRawAsyncServer.cached_queries(ctxt: THttpServerRequest): cardinal;
var
  i: PtrInt;
  res: TOrmWorlds;
  gen: PLecuyer;
begin
  SetLength(res, GetQueriesParamValue(ctxt, 'COUNT='));
  gen := Lecuyer;
  for i := 0 to length(res) - 1 do
    res[i] := fCachedWorldsTable.Get(ComputeRandomWorld(gen));
  ctxt.SetOutJson(@res, TypeInfo(TOrmWorlds));
  result := HTTP_SUCCESS;
end;

function OrmFortuneCompareByMessage(const A, B): integer;
begin
  result := StrComp(pointer(TOrmFortune(A).Message), pointer(TOrmFortune(B).Message));
end;

function TRawAsyncServer.fortunes(ctxt: THttpServerRequest): cardinal;
var
  list: TOrmFortunes;
  new: TOrmFortune;
  arr: TDynArray;
begin
  result := HTTP_SERVERERROR;
  arr.Init(TypeInfo(TOrmFortunes), list);
  if fStore.Orm.RetrieveListObjArray(list, TOrmFortune, '', []) then
    try
      new := TOrmFortune.Create;
      new.Message := FORTUNES_MESSAGE;
      arr.Add(new);
      arr.Sort(OrmFortuneCompareByMessage);
      ctxt.OutContent := fTemplate.RenderDataArray(arr);
      ctxt.OutContentType := HTML_CONTENT_TYPE;
      result := HTTP_SUCCESS;
    finally
      arr.Clear;
    end;
end;

function TRawAsyncServer.updates(ctxt: THttpServerRequest): cardinal;
var
  i: PtrInt;
  res: TOrmWorlds;
  w: TOrmWorld;
  b: TRestBatch;
  gen: PLecuyer;
begin
  result := HTTP_SERVERERROR;
  SetLength(res, GetQueriesParamValue(ctxt));
  b := TRestBatch.Create(fStore.ORM, TOrmWorld, {transrows=}0,
    [boExtendedJson, boNoModelEncoding, boPutNoCacheFlush]);
  try
    gen := Lecuyer;
    for i := 0 to length(res) - 1 do
    begin
      w := TOrmWorld.Create;
      res[i] := w;
      if not fStore.Orm.Retrieve(ComputeRandomWorld(gen), w) then
        exit;
      w.RandomNumber := ComputeRandomWorld(gen);
      b.Update(w);
    end;
    result := b.Send;
    if result = HTTP_SUCCESS then
      ctxt.SetOutJson(@res, TypeInfo(TOrmWorlds));
  finally
    b.Free;
    ObjArrayClear(res);
  end;
end;

function TRawAsyncServer.rawdb(ctxt: THttpServerRequest): cardinal;
var
  conn: TSqlDBConnection;
  stmt: ISqlDBStatement;
begin
  result := HTTP_SERVERERROR;
  conn := fDbPool.ThreadSafeConnection;
  stmt := conn.NewStatementPrepared(WORLD_READ_SQL, true, true);
  stmt.Bind(1, ComputeRandomWorld(Lecuyer));
  stmt.ExecutePrepared;
  if stmt.Step then
  begin
    ctxt.SetOutJson(
      '{"id":%,"randomNumber":%}', [stmt.ColumnInt(0), stmt.ColumnInt(1)]);
    result := HTTP_SUCCESS;
    stmt.ReleaseRows;
  end;
  stmt := nil;
end;

function TRawAsyncServer.rawqueries(ctxt: THttpServerRequest): cardinal;
var
  res: TWorlds;
begin
  if not GetRawRandomWorlds(GetQueriesParamValue(ctxt), res) then
    exit(HTTP_SERVERERROR);
  ctxt.SetOutJson(@res, TypeInfo(TWorlds));
  result := HTTP_SUCCESS;
end;

function TRawAsyncServer.rawcached(ctxt: THttpServerRequest): cardinal;
var
  i: PtrInt;
  res: TOrmWorlds;
  gen: PLecuyer;
begin
  SetLength(res, GetQueriesParamValue(ctxt, 'COUNT='));
  gen := Lecuyer;
  for i := 0 to length(res) - 1 do
    res[i] := fRawCache[ComputeRandomWorld(gen) - 1];
  ctxt.SetOutJson(@res, TypeInfo(TOrmWorlds));
  result := HTTP_SUCCESS;
end;

function TRawAsyncServer.rawfortunes(ctxt: THttpServerRequest): cardinal;
var
  conn: TSqlDBConnection;
  stmt: ISqlDBStatement;
begin
  conn := fDbPool.ThreadSafeConnection;
  stmt := conn.NewStatementPrepared(FORTUNES_SQL, true, true);
  stmt.ExecutePrepared;
  result := ComputeRawFortunes(stmt.Instance, ctxt);
end;

var
  LastComputeUpdateSql: RawUtf8;
  LastComputeUpdateSqlCnt: integer;
  LastComputeUpdateSqlSafe: TLightLock;

function ComputeUpdateSql(cnt: integer): RawUtf8;
var
  i: integer;
  W: TTextWriter;
  tmp: TTextWriterStackBuffer;
begin
  LastComputeUpdateSqlSafe.Lock;
  if cnt <> LastComputeUpdateSqlCnt then
  begin
    // update table set .. from values (), (), ... where id = id
    // we won't include it in the ORM but only for our RAW results
    LastComputeUpdateSqlCnt := cnt;
    W := TTextWriter.CreateOwnedStream(tmp{%H-});
    try
      W.AddShort('UPDATE world SET randomNumber = v.randomNumber FROM (VALUES');
      for i := 1 to cnt do
        W.AddShort('(?::integer, ?::integer),');
      W.CancelLastComma;
      W.AddShort(' order by 1) AS v (id, randomNumber) WHERE world.id = v.id');
      W.SetText(LastComputeUpdateSql);
    finally
      W.Free;
    end;
  end;
  result := LastComputeUpdateSql;
  LastComputeUpdateSqlSafe.UnLock;
end;

function TRawAsyncServer.rawupdates(ctxt: THttpServerRequest): cardinal;
var
  cnt, i: PtrInt;
  res: TWorlds;
  ids, nums: TInt64DynArray;
  gen: PLecuyer;
  conn: TSqlDBConnection;
  stmt: ISqlDBStatement;
begin
  result := HTTP_SERVERERROR;
  conn := fDbPool.ThreadSafeConnection;
  cnt := getQueriesParamValue(ctxt);
  if not getRawRandomWorlds(cnt, res) then
    exit;
  // generate new randoms
  gen := Lecuyer;
  for i := 0 to cnt - 1 do
    res[i].randomNumber := ComputeRandomWorld(gen);
  if cnt > 20 then
  begin
    // fill parameters arrays for update with nested select (PostgreSQL only)
    setLength(ids{%H-}, cnt);
    setLength(nums{%H-}, cnt);
    for i := 0 to cnt - 1 do
    begin
      ids[i] := res[i].id;
      nums[i] := res[i].randomNumber;
    end;
    stmt := conn.NewStatementPrepared(WORLD_UPDATE_SQLN, false, true);
    stmt.BindArray(1, ids);
    stmt.BindArray(2, nums);
  end
  else
  begin
    // fill parameters for update up to 20 items as VALUES(?,?),(?,?),...
    stmt := conn.NewStatementPrepared(ComputeUpdateSql(cnt), false, true);
    for i := 0 to cnt - 1 do
    begin
      stmt.Bind(i * 2 + 1, res[i].id);
      stmt.Bind(i * 2 + 2, res[i].randomNumber);
    end;
  end;
  stmt.ExecutePrepared;
  ctxt.SetOutJson(@res, TypeInfo(TWorlds));
  result := HTTP_SUCCESS;
end;

// asynchronous PostgreSQL pipelined DB access

const
  // follow TFB requirements, and potential patched libpq
  ASYNC_OPT = [asoForceConnectionFlush, asoForcePipelineSync];

function TRawAsyncServer.asyncdb(ctxt: THttpServerRequest): cardinal;
begin
  with fDbPool.Async.PrepareLocked(WORLD_READ_SQL, {res=}true, ASYNC_OPT) do
  try
    Bind(1, ComputeRandomWorld(Lecuyer));
    ExecuteAsync(ctxt, OnAsyncDb);
  finally
    UnLock;
  end;
  result := ctxt.SetAsyncResponse;
end;

procedure TRawAsyncServer.OnAsyncDb(Statement: TSqlDBPostgresAsyncStatement;
  Context: TObject);
var
  ctxt: THttpServerRequest absolute Context;
begin
  if (Statement = nil) or
     not Statement.Step then
    ctxt.ErrorMessage := 'asyncdb failed'
  else
    ctxt.SetOutJson('{"id":%,"randomNumber":%}',
      [Statement.ColumnInt(0), Statement.ColumnInt(1)]);
  ctxt.OnAsyncResponse(ctxt);
end;

function TRawAsyncServer.asyncfortunes(ctxt: THttpServerRequest): cardinal;
begin
  fDbPool.Async.PrepareLocked(FORTUNES_SQL, {res=}true, ASYNC_OPT).
    ExecuteAsyncNoParam(ctxt, OnAsyncFortunes);
  result := ctxt.SetAsyncResponse;
end;

procedure TRawAsyncServer.OnAsyncFortunes(Statement: TSqlDBPostgresAsyncStatement;
  Context: TObject);
var
  ctxt: THttpServerRequest absolute Context;
begin
  ctxt.OnAsyncResponse(ctxt, ComputeRawFortunes(Statement, ctxt));
end;

type
  // simple state machine used for /asyncqueries and /asyncupdates
  TAsyncWorld = class
  public
    request: THttpServerRequest;
    res: TWorlds;
    count, current: integer;
    update: TSqlDBPostgresAsyncStatement; // prepared before any callback
    async: TSqlDBPostgresAsync;
    function Queries(server: TRawAsyncServer; ctxt: THttpServerRequest): cardinal;
    function Updates(server: TRawAsyncServer; ctxt: THttpServerRequest): cardinal;
    procedure DoUpdates;
    procedure OnQueries(Statement: TSqlDBPostgresAsyncStatement; Context: TObject);
    procedure OnRes({%H-}Statement: TSqlDBPostgresAsyncStatement; Context: TObject);
  end;

function TRawAsyncServer.asyncqueries(ctxt: THttpServerRequest): cardinal;
begin
  result := TAsyncWorld.Create.Queries(self, ctxt);
end;

function TRawAsyncServer.asyncupdates(ctxt: THttpServerRequest): cardinal;
begin
  result := TAsyncWorld.Create.Updates(self, ctxt);
end;


{ TAsyncWorld }

function TAsyncWorld.Queries(server: TRawAsyncServer; ctxt: THttpServerRequest): cardinal;
var
  n: integer;
  opt: TSqlDBPostgresAsyncStatementOptions; // forced options for modified libpq
  gen: PLecuyer;
  select: TSqlDBPostgresAsyncStatement;
begin
  request := ctxt;
  if async = nil then
    async := server.fDbPool.Async;
  if count = 0 then
    count := getQueriesParamValue(ctxt);
  SetLength(res, count); // count is > 0
  select := async.PrepareLocked(WORLD_READ_SQL, {res=}true, ASYNC_OPT);
  opt := ASYNC_OPT - [asoForceConnectionFlush];
  n := count;
  gen := Lecuyer;
  repeat
    dec(n);
    select.Bind(1, ComputeRandomWorld(gen));
    if n = 0 then // last item should include asoForceConnectionFlush (if set)
      opt := ASYNC_OPT;
    select.ExecuteAsync(ctxt, OnQueries, @opt);
  until n = 0;
  select.UnLock;
  result := ctxt.SetAsyncResponse;
end;

function TAsyncWorld.Updates(server: TRawAsyncServer; ctxt: THttpServerRequest): cardinal;
begin
  async := server.fDbPool.Async;
  count := getQueriesParamValue(ctxt);
  update := async.Prepare(WORLD_UPDATE_SQLN, false, ASYNC_OPT);
  result := Queries(server, ctxt);
end;

procedure TAsyncWorld.OnQueries(Statement: TSqlDBPostgresAsyncStatement;
  Context: TObject);
begin
  if (Statement <> nil) and
     Statement.Step then
    with res[current] do
    begin
      id := Statement.ColumnInt(0);
      randomNumber := Statement.ColumnInt(1);
    end;
  inc(current);
  if current = count then // we retrieved all SELECT
    if Assigned(update) then
      DoUpdates
    else
      OnRes(Statement, Context);
end;

procedure TAsyncWorld.DoUpdates;
var
  i: PtrInt;
  params: TIntegerDynArray;
  gen: PLecuyer;
begin
  gen := Lecuyer;
  for i := 0 to count - 1 do
    res[i].randomNumber := ComputeRandomWorld(gen);
  SetLength(params, count);
  for i := 0 to count - 1 do
    params[i] := res[i].id;
  update.BindArrayInt32(1, params);
  for i := 0 to count - 1 do
    params[i] := res[i].randomNumber;
  update.BindArrayInt32(2, params);
  update.ExecuteAsync(request, OnRes);
end;

procedure TAsyncWorld.OnRes(Statement: TSqlDBPostgresAsyncStatement;
  Context: TObject);
begin
  request.SetOutJson(@res, TypeInfo(TWorlds));
  request.OnAsyncResponse(Context as THttpServerRequest);
  Free; // we don't need this state machine any more
end;


var
  rawServers: array of TRawAsyncServer;
  threads, servers, i, k, cpuIdx, cpuCount: integer;
  pinServers2Cores: boolean;
  cpuMask: TCpuSet;
  flags: THttpServerOptions;
begin
  // setup logs
  {$ifdef WITH_LOGS}
  TSynLog.Family.Level := LOG_VERBOSE; // disable logs for benchmarking
  TSynLog.Family.HighResolutionTimestamp := true;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;
  TSynLog.Family.AutoFlushTimeOut := 1;
  {$else}
  SynDBLog := nil; // slightly faster: no need to check log level
  {$endif WITH_LOGS}

  // register some RTTI for records JSON serialization
  Rtti.RegisterFromText([
    TypeInfo(TMessageRec), 'message:PUtf8Char',
    TypeInfo(TWorldRec),   'id,randomNumber:cardinal',
    TypeInfo(TFortune),    'id:PtrUInt message:PUtf8Char']);

  // compute default execution context from HW information
  cpuCount := CurrentCpuSet(cpuMask); // may run from a "taskset" command
  if cpuCount >= 6 then
  begin
    // high-end CPU would scale better using several listeners (one per core)
    // see https://synopse.info/forum/viewtopic.php?pid=39263#p39263
    servers := cpuCount;
    threads := 8;
    pinServers2Cores := true;
    if GetEnvironmentVariable('TFB_TEST_NAME') = 'mormot-postgres-async' then
    begin
      // asynchronus test
      servers := cpuCount;
      threads := 8;
    end
    else
    if GetEnvironmentVariable('TFB_TEST_NAME') = 'mormot-postgres-async2' then
    begin
      // asynchronus test with single listener socket and no CPU pinning
      servers := 1;
      threads := cpuCount * 4;
      pinServers2Cores := false;
    end;
  end
  else
  begin
    // simple CPU will have a single instance and a few threads per core
    servers := 1;
    threads := cpuCount * 4;
    pinServers2Cores := false;
  end;

  // parse command line parameters
  with Executable.Command do
  begin
    ExeDescription := 'TFB Server using mORMot 2';
    if Option(['p', 'pin'], 'pin each server to a CPU') then
      pinServers2Cores := true;
    if Option('nopin', 'disable the CPU pinning') then
      pinServers2Cores := false; // no option would keep the default boolean
    Get(['s', 'servers'], servers, '#count of servers (listener sockets)', servers);
    Get(['t', 'threads'], threads, 'per-server thread pool #size', threads);
    if ConsoleHelpFailed('TFB Server using mORMot 2') then
      exit;
  end;

  // start the server instance(s), in hsoReusePort mode if needed
  flags := [];
  if servers > 1 then
    include(flags, hsoReusePort) // allow several bindings on the same port
  else
    pinServers2Cores := false;   // pinning a single server won't make any sense
  SetLength(rawServers{%H-}, servers);
  cpuIdx := -1; // do not pin to CPU by default
  for i := 0 to servers - 1 do
  begin
    if pinServers2Cores then
    begin
      k := i mod cpuCount;
      cpuIdx := -1;
      // find real CPU index according to the cpuMask
      repeat
        inc(cpuIdx);
        if GetBit(cpuMask, cpuIdx) then
          dec(k);
      until k = -1;
      writeln('Pin #', i, ' server to #', cpuIdx, ' CPU');
    end;
    rawServers[i] := TRawAsyncServer.Create(threads, flags, cpuIdx)
  end;

  try
    // display some information and wait for SIGTERM
    writeln;
    writeln(rawServers[0].fHttpServer.ClassName,
     ' running on localhost:', rawServers[0].fHttpServer.SockPort);
    writeln(' num servers=', servers,
            ', threads per server=', threads,
            ', total threads=', threads * servers,
            ', total CPU=', SystemInfo.dwNumberOfProcessors,
            ', accessible CPU=', cpuCount,
            ', pinned=', pinServers2Cores,
            ', db=', rawServers[0].fDbPool.DbmsEngineName);
    writeln(' options=', GetSetName(TypeInfo(THttpServerOptions), flags));
    writeln('Press [Enter] or Ctrl+C or send SIGTERM to terminate');
    ConsoleWaitForEnterKey;
    //TSynLog.Family.Level := LOG_VERBOSE; // enable shutdown logs for debug
    if servers = 1 then
      writeln(ObjectToJsonDebug(rawServers[0].fHttpServer))
    else
    begin
      writeln('Per-server accepted connections:');
      for i := 0 to servers - 1 do
        write(' ', rawServers[i].fHttpServer.Async.Accepted);
      writeln(#10'Please wait: Shutdown ', servers, ' servers and ',
        threads * servers, ' threads');
    end;
  finally
    // clear all server instance(s)
    ObjArrayClear(rawServers);
  end;
  write('Shutdown complete'#10);
  {$ifdef FPC_X64MM}
  WriteHeapStatus(' ', 16, 8, {compileflags=}true);
  {$endif FPC_X64MM}
end.
