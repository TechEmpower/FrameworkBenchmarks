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
    message: RawUtf8;
  end;
  TWorldRec = packed record
    id: integer;
    randomNumber: integer;
  end;
  TWorlds = array of TWorldRec;
  TFortune = packed record
    id: integer;
    message: RawUtf8;
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
    fDbPool: TSqlDBConnectionProperties;
    fModel: TOrmModel;
    fStore: TRestServerDB;
    fTemplate: TSynMustache;
    fCachedWorldsTable: POrmCacheTable;
    fAsyncWorldRead, fAsyncFortunesRead: TSqlDBPostgresAsyncStatement;
    fAsyncWorldUpdate: TSqlDBPostgresAsyncStatement;
    procedure OnAsyncDb(Statement: TSqlDBPostgresAsyncStatement; Context: TObject);
    procedure OnAsyncFortunes(Statement: TSqlDBPostgresAsyncStatement; Context: TObject);
    // pipelined reading as used by /rawqueries and /rawupdates
    function GetRawRandomWorlds(cnt: PtrInt; out res: TWorlds): boolean;
    function ComputeRawFortunes(stmt: TSqlDBStatement; ctxt: THttpServerRequest): integer;
  public
    constructor Create(threadCount: integer; flags: THttpServerOptions;
      pin2Core: integer = -1); reintroduce;
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
    '(SELECT unnest(?::bigint[]), unnest(?::bigint[]) order by 1) as v(id, r)' +
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


function ComputeRandomWorld: integer; inline;
begin
  result := Random32(WORLD_COUNT) + 1;
end;

function GetQueriesParamValue(ctxt: THttpServerRequest;
  const search: RawUtf8 = 'QUERIES='): cardinal;
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
  with (fDBPool as TSqlDBPostgresConnectionProperties).Async do
  begin
    fAsyncWorldRead := NewStatement(WORLD_READ_SQL,
      [asoForceConnectionFlush, asoForcePipelineSync]);
    fAsyncFortunesRead := NewStatement(FORTUNES_SQL,
      [asoForceConnectionFlush, asoForcePipelineSync]);
    fAsyncWorldUpdate := NewStatement(WORLD_UPDATE_SQLN,
      [asoForceConnectionFlush, asoForcePipelineSync, asoExpectNoResult]);
    // no SetThreadCpuAffinity(fAsyncWorldRead.Owner.Thread, pin2Core) needed
  end;
  // pre-fill the ORM
  if fStore.Server.Cache.SetCache(TOrmCachedWorld) then
    fStore.Server.Cache.FillFromQuery(TOrmCachedWorld, '', []);
  fCachedWorldsTable := fStore.Orm.Cache.Table(TOrmCachedWorld);
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
     {$ifdef WITH_LOGS}
     hsoLogVerbose,
     {$endif WITH_LOGS}
     hsoIncludeDateHeader  // required by TPW General Test Requirements #5
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
  fDBPool.free;
  inherited Destroy;
end;

// query DB world table for /rawqueries and /rawupdates endpoints

function TRawAsyncServer.GetRawRandomWorlds(cnt: PtrInt; out res: TWorlds): boolean;
var
  conn: TSqlDBConnection;
  stmt: ISqlDBStatement;
  pConn: TSqlDBPostgresConnection absolute conn;
  pStmt: TSqlDBPostgresStatement;
  i: PtrInt;
begin
  result := false;
  SetLength(res{%H-}, cnt);
  conn := fDbPool.ThreadSafeConnection;
  // specific code to use PostgresSQL pipelining mode
  // see test_nosync in
  // https://github.com/postgres/postgres/blob/master/src/test/modules/libpq_pipeline/libpq_pipeline.c
  stmt := conn.NewStatementPrepared(WORLD_READ_SQL, true, true);
  pConn.EnterPipelineMode;
  pStmt := TSqlDBPostgresStatement(stmt.Instance);
  for i := 0 to cnt - 1 do
  begin
    pStmt.Bind(1, ComputeRandomWorld);
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
begin
  result := HTTP_BADREQUEST;
  if stmt = nil then
    exit;
  arr.Init(TypeInfo(TFortunes), list, @n);
  while stmt.Step do
  begin
    f := arr.NewPtr;
    f.id := stmt.ColumnInt(0);
    f.message := stmt.ColumnUtf8(1);
  end;
  f := arr.NewPtr;
  f.id := 0;
  f.message := FORTUNES_MESSAGE;
  arr.Sort(FortuneCompareByMessage);
  ctxt.OutContent := fTemplate.RenderDataArray(arr);
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
  msgRec.message := HELLO_WORLD;
  ctxt.SetOutJson(@msgRec, TypeInfo(TMessageRec));
  result := HTTP_SUCCESS;
end;

function TRawAsyncServer.db(ctxt: THttpServerRequest): cardinal;
var
  w: TOrmWorld;
begin
  w := TOrmWorld.Create(fStore.Orm, ComputeRandomWorld);
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
begin
  SetLength(res, GetQueriesParamValue(ctxt, 'QUERIES='));
  for i := 0 to length(res) - 1 do
    res[i] := TOrmWorld.Create(fStore.Orm, ComputeRandomWorld);
  ctxt.SetOutJson(@res, TypeInfo(TOrmWorlds));
  ObjArrayClear(res);
  result := HTTP_SUCCESS;
end;

function TRawAsyncServer.cached_queries(ctxt: THttpServerRequest): cardinal;
var
  i: PtrInt;
  res: TOrmWorlds;
begin
  SetLength(res, GetQueriesParamValue(ctxt, 'COUNT='));
  for i := 0 to length(res) - 1 do
    res[i] := fCachedWorldsTable.Get(ComputeRandomWorld);
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
begin
  result := HTTP_SERVERERROR;
  SetLength(res, GetQueriesParamValue(ctxt));
  b := TRestBatch.Create(fStore.ORM, TOrmWorld, {transrows=}0,
    [boExtendedJson, boNoModelEncoding, boPutNoCacheFlush]);
  try
    for i := 0 to length(res) - 1 do
    begin
      w := TOrmWorld.Create;
      res[i] := w;
      if not fStore.Orm.Retrieve(ComputeRandomWorld, w) then
        exit;
      w.RandomNumber := ComputeRandomWorld;
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
  stmt.Bind(1, ComputeRandomWorld);
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
      for i := 1 to cnt do begin
        W.AddShort('(?::integer, ?::integer)');
        W.Add(',');
      end;
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
  conn: TSqlDBConnection;
  stmt: ISqlDBStatement;
begin
  result := HTTP_SERVERERROR;
  conn := fDbPool.ThreadSafeConnection;
  cnt := getQueriesParamValue(ctxt);
  if not getRawRandomWorlds(cnt, res) then
    exit;
  // generate new randoms
  for i := 0 to cnt - 1 do
    res[i].randomNumber := ComputeRandomWorld;
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

function TRawAsyncServer.asyncdb(ctxt: THttpServerRequest): cardinal;
begin
  fAsyncWorldRead.Lock;
  try
    fAsyncWorldRead.Bind(1, ComputeRandomWorld);
    fAsyncWorldRead.ExecuteAsync(ctxt, OnAsyncDb);
  finally
    fAsyncWorldRead.UnLock;
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
  fAsyncFortunesRead.ExecuteAsyncNoParam(ctxt, OnAsyncFortunes);
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
    server: TRawAsyncServer;
    request: THttpServerRequest;
    res: TWorlds;
    count: PtrInt;
    fromupdates: boolean;
    function Queries(owner: TRawAsyncServer; ctxt: THttpServerRequest): cardinal;
    function Updates(owner: TRawAsyncServer; ctxt: THttpServerRequest): cardinal;
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

function TAsyncWorld.Queries(owner: TRawAsyncServer; ctxt: THttpServerRequest): cardinal;
var
  n: PtrInt;
  opt: TSqlDBPostgresAsyncStatementOptions; // for modified libpq
begin
  server := owner;
  request := ctxt;
  n := getQueriesParamValue(ctxt);
  SetLength(res, n); // n is > 0
  server.fAsyncWorldRead.Lock;
  try
    opt := server.fAsyncWorldRead.AsyncOptions - [asoForceConnectionFlush];
    repeat
      dec(n);
      server.fAsyncWorldRead.Bind(1, ComputeRandomWorld);
      if n = 0 then // last item
        opt := server.fAsyncWorldRead.AsyncOptions;
      server.fAsyncWorldRead.ExecuteAsync(ctxt, OnQueries, @opt);
    until n = 0;
  finally
    server.fAsyncWorldRead.UnLock;
  end;
  result := ctxt.SetAsyncResponse;
end;

function TAsyncWorld.Updates(owner: TRawAsyncServer;
  ctxt: THttpServerRequest): cardinal;
begin
  fromupdates := true;
  result := Queries(owner, ctxt);
end;

procedure TAsyncWorld.OnQueries(Statement: TSqlDBPostgresAsyncStatement;
  Context: TObject);
begin
  if (Statement <> nil) and
     Statement.Step then
    with res[count] do
    begin
      id := Statement.ColumnInt(0);
      randomNumber := Statement.ColumnInt(1);
    end;
  inc(count);
  if count = length(res) then // we retrieved all SELECT
    if fromupdates then
      DoUpdates
    else
      OnRes(Statement, Context);
end;

procedure TAsyncWorld.DoUpdates;
var
  i: PtrInt;
  ids, nums: TInt64DynArray;
begin
  setLength(ids{%H-}, count);
  setLength(nums{%H-}, count);
  for i := 0 to count - 1 do
  with res[i] do
  begin
    randomNumber := ComputeRandomWorld;
    ids[i] := id;
    nums[i] := randomNumber;
  end;
  // note: no need of server.fAsyncWorldUpdate.Lock/UnLock inside the callbacks
  server.fAsyncWorldUpdate.BindArray(1, ids);
  server.fAsyncWorldUpdate.BindArray(2, nums);
  server.fAsyncWorldUpdate.ExecuteAsync(request, OnRes);
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
    TypeInfo(TMessageRec), 'message:RawUtf8',
    TypeInfo(TWorldRec),   'id,randomNumber:integer',
    TypeInfo(TFortune),    'id:integer message:RawUtf8']);

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
      servers := cpuCount * 2;
      threads := 1;
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
    if Option(['?', 'help'], 'display this message') then
    begin
      ConsoleWrite(FullDescription);
      exit;
    end;
    if ConsoleWriteUnknown then
      exit;
  end;

  // start the server instance(s), in hsoReusePort mode if needed
  flags := [];
  if servers > 1 then
    include(flags, hsoReusePort) // allow several bindings on the same port
  else
    pinServers2Cores := false;   // don't make any sense
  SetLength(rawServers{%H-}, servers);
  cpuIdx := -1; // do not pin to CPU by default
  for i := 0 to servers - 1 do begin
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
      writeln(ObjectToJsonDebug(rawServers[0].fHttpServer,
        [woDontStoreVoid, woHumanReadable]))
    else
    begin
      writeln('Per-server accepted connections:');
      for i := 0 to servers - 1 do
        write(' ', rawServers[i].fHttpServer.Async.Accepted);
      writeln(#10'Please wait: Shutdown ', servers, ' servers');
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
