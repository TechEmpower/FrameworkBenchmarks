program raw;

{
TechEmpower framework benchmarks implementation
See https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview
}

{$I mormot.defines.inc}

{.$define WITH_LOGS}
// logging is fine for debugging, less for benchmarking ;)

uses
  {$I mormot.uses.inc} // include mormot.core.fpcx64mm or mormot.core.fpclibcmm
  sysutils,
  classes,
  BaseUnix,
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
  mormot.orm.sql,
  mormot.db.core,
  mormot.db.raw.sqlite3,
  mormot.db.raw.sqlite3.static,
  mormot.rest.sqlite3,
  mormot.net.http,
  mormot.net.server,
  mormot.net.async,
  mormot.db.sql,
  mormot.db.sql.postgres;

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
  TOrmWorldClass = class of TOrmWorld;
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
  private
    fHttpServer: THttpAsyncServer;
    fDbPool: TSqlDBConnectionProperties;
    fModel: TOrmModel;
    fStore: TRestServerDB;
    fTemplate: TSynMustache;
  protected
    // as used by rawqueries and rawupdates
    function getRawRandomWorlds(cnt: PtrInt; out res: TWorlds): boolean;
    // implements /queries and /cached-queries endpoints
    function doqueries(ctxt: THttpServerRequestAbstract; orm: TOrmWorldClass;
      const search: RawUtf8): cardinal;
  public
    constructor Create(threadCount: integer; flags: THttpServerOptions); reintroduce;
    destructor Destroy; override;
  published
    // all service URI are implemented by these published methods using RTTI
    function plaintext(ctxt: THttpServerRequestAbstract): cardinal;
    function json(ctxt: THttpServerRequestAbstract): cardinal;
    function db(ctxt: THttpServerRequestAbstract): cardinal;
    function queries(ctxt: THttpServerRequestAbstract): cardinal;
    function cached_queries(ctxt: THttpServerRequestAbstract): cardinal;
    function fortunes(ctxt: THttpServerRequestAbstract): cardinal;
    function updates(ctxt: THttpServerRequestAbstract): cardinal;
    function rawdb(ctxt: THttpServerRequestAbstract): cardinal;
    function rawqueries(ctxt: THttpServerRequestAbstract): cardinal;
    function rawfortunes(ctxt: THttpServerRequestAbstract): cardinal;
    function rawupdates(ctxt: THttpServerRequestAbstract): cardinal;
  end;

const
  TEXT_CONTENT_TYPE_NO_ENCODING: RawUtf8 = 'text/plain';
  HELLO_WORLD: RawUtf8 = 'Hello, World!';
  WORLD_COUNT = 10000;

  WORLD_READ_SQL = 'select id,randomNumber from World where id=?';
  WORLD_UPDATE_SQLN ='update World as t set randomNumber = v.r from ' +
    '(SELECT unnest(?::bigint[]), unnest(?::bigint[]) order by 1) as v(id, r)' +
    ' where t.id = v.id';
  FORTUNES_SQL = 'select id,message from Fortune';

  FORTUNES_MESSAGE = 'Additional fortune added at request time.';
  FORTUNES_TPL = '<!DOCTYPE html>' +
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


function RandomWorld: integer; inline;
begin
  result := Random32(WORLD_COUNT) + 1;
end;

function getQueriesParamValue(ctxt: THttpServerRequestAbstract;
  const search: RawUtf8 = 'QUERIES='): cardinal;
begin
  if not ctxt.UrlParam(search, result) then
    result := 1
  else if result > 500 then
    result := 500
  else if result < 1 then
    result := 1;
end;


{ TRawAsyncServer }

constructor TRawAsyncServer.Create(
  threadCount: integer; flags: THttpServerOptions);
begin
  inherited Create;
  fDbPool := TSqlDBPostgresConnectionProperties.Create(
    'tfb-database:5432', 'hello_world', 'benchmarkdbuser', 'benchmarkdbpass');
  fModel := TOrmModel.Create([TOrmWorld, TOrmFortune, TOrmCachedWorld]);
  OrmMapExternal(fModel, [TOrmWorld, TOrmFortune], fDbPool);
  // CachedWorld table doesn't exists in DB, but should as read in requirements.
  // Use world table as in other implementations.
  OrmMapExternal(fModel, TOrmCachedWorld, fDbPool, 'world');
  fStore := TRestServerDB.Create(fModel, SQLITE_MEMORY_DATABASE_NAME);
  fStore.NoAjaxJson := true;
  fStore.Server.CreateMissingTables; // create SQlite3 virtual tables
  if fStore.Server.Cache.SetCache(TOrmCachedWorld) then
    fStore.Server.Cache.FillFromQuery(TOrmCachedWorld, '', []);
  fTemplate := TSynMustache.Parse(FORTUNES_TPL);
  fHttpServer := THttpAsyncServer.Create(
    '8080', nil, nil, '', threadCount,
    5 * 60 * 1000,         // 5 minutes keep alive connections
    [hsoNoXPoweredHeader,  // not needed for a benchmark
     hsoHeadersInterning,  // reduce memory contention for /plaintext and /json
     hsoNoStats,           // disable low-level statistic counters
     //hsoThreadCpuAffinity, // better scaling of /plaintext in some cases
     hsoReusePort,         // allow several processes binding on the same port
     {$ifdef WITH_LOGS}
     hsoLogVerbose,
     {$endif WITH_LOGS}
     hsoIncludeDateHeader  // required by TPW General Test Requirements #5
    ] + flags);
  fHttpServer.HttpQueueLength := 10000; // needed e.g. from wrk/ab benchmarks
  fHttpServer.Route.RunMethods([urmGet], self);
  // writeln(fHttpServer.Route.Tree[urmGet].ToText);
  fHttpServer.WaitStarted; // raise exception e.g. on binding issue
end;

destructor TRawAsyncServer.Destroy;
begin
  fHttpServer.Free;
  fStore.Free;
  fModel.Free;
  fDBPool.free;
  inherited Destroy;
end;

function TRawAsyncServer.plaintext(ctxt: THttpServerRequestAbstract): cardinal;
begin
  ctxt.OutContentType := TEXT_CONTENT_TYPE_NO_ENCODING;
  ctxt.OutContent := HELLO_WORLD;
  result := HTTP_SUCCESS;
end;

function TRawAsyncServer.json(ctxt: THttpServerRequestAbstract): cardinal;
var
  msgRec: TMessageRec;
begin
  msgRec.message := HELLO_WORLD;
  ctxt.SetOutJson(SaveJson(msgRec, TypeInfo(TMessageRec)));
  result := HTTP_SUCCESS;
end;

function TRawAsyncServer.rawdb(ctxt: THttpServerRequestAbstract): cardinal;
var
  conn: TSqlDBConnection;
  stmt: ISQLDBStatement;
begin
  result := HTTP_SERVERERROR;
  conn := fDbPool.ThreadSafeConnection;
  stmt := conn.NewStatementPrepared(WORLD_READ_SQL, true, true);
  stmt.Bind(1, RandomWorld);
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

function TRawAsyncServer.db(ctxt: THttpServerRequestAbstract): cardinal;
var
  w: TOrmWorld;
begin
  w := TOrmWorld.Create(fStore.Orm, RandomWorld);
  try
    ctxt.SetOutJson('{"id":%,"randomNumber":%}', [w.IDValue, w.randomNumber]);
    result := HTTP_SUCCESS;
  finally
    w.Free;
  end;
end;

function TRawAsyncServer.queries(ctxt: THttpServerRequestAbstract): cardinal;
begin
  result := doqueries(ctxt, TOrmWorld, 'QUERIES=');
end;

function TRawAsyncServer.cached_queries(ctxt: THttpServerRequestAbstract): cardinal;
begin
  result := doqueries(ctxt, TOrmCachedWorld, 'COUNT=');
end;

function TRawAsyncServer.getRawRandomWorlds(cnt: PtrInt; out res: TWorlds): boolean;
var
  conn: TSqlDBConnection;
  stmt: ISQLDBStatement;
  pConn: TSqlDBPostgresConnection absolute conn;
  pStmt: TSqlDBPostgresStatement;
  i: PtrInt;
begin
  result := false;
  SetLength(res{%H-}, cnt);
  conn := fDbPool.ThreadSafeConnection;
  if not conn.IsConnected then
    conn.Connect;
  // specific code to use PostgresSQL pipelining mode
  // see test_multi_pipelines in
  // https://github.com/postgres/postgres/blob/master/src/test/modules/libpq_pipeline/libpq_pipeline.c
  stmt := conn.NewStatementPrepared(WORLD_READ_SQL, true, true);
  //conn.StartTransaction;
  pConn.EnterPipelineMode;
  pStmt := (stmt as TSqlDBPostgresStatement);
  for i := 0 to cnt - 1 do
  begin
    stmt.Bind(1, RandomWorld);
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
  //conn.commit;
  result := true;
end;

function TRawAsyncServer.rawqueries(ctxt: THttpServerRequestAbstract): cardinal;
var
  cnt: PtrInt;
  res: TWorlds;
begin
  cnt := getQueriesParamValue(ctxt);
  if not getRawRandomWorlds(cnt, res) then
    exit(HTTP_SERVERERROR);
  ctxt.SetOutJson(SaveJson(res, TypeInfo(TWorlds)));
  result := HTTP_SUCCESS;
end;

function TRawAsyncServer.doqueries(ctxt: THttpServerRequestAbstract;
  orm: TOrmWorldClass; const search: RawUtf8): cardinal;
var
  cnt, i: PtrInt;
  res: TWorlds;
  w: TOrmWorld;
begin
  result := HTTP_SERVERERROR;
  cnt := getQueriesParamValue(ctxt, search);
  SetLength(res, cnt);
  w := orm.Create; // TOrmWorld or TOrmCachedWorld
  try
    for i := 0 to cnt - 1 do
    begin
      if not fStore.Orm.Retrieve(RandomWorld, w) then
        exit;
      res[i].id := w.IDValue;
      res[i].randomNumber := w.RandomNumber;
    end;
  finally
    w.Free;
  end;
  ctxt.SetOutJson(SaveJson(res, TypeInfo(TWorlds)));
  result := HTTP_SUCCESS;
end;

function OrmFortuneCompareByMessage(const A, B): integer;
begin
  result := StrComp(pointer(TOrmFortune(A).Message), pointer(TOrmFortune(B).Message));
end;

function TRawAsyncServer.fortunes(ctxt: THttpServerRequestAbstract): cardinal;
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

function FortuneCompareByMessage(const A, B): integer;
begin
  result := StrComp(pointer(TFortune(A).message), pointer(TFortune(B).message));
end;

function TRawAsyncServer.rawfortunes(ctxt: THttpServerRequestAbstract): cardinal;
var
  conn: TSqlDBConnection;
  stmt: ISQLDBStatement;
  list: TFortunes;
  arr: TDynArray;
  n: integer;
  f: ^TFortune;
begin
  conn := fDbPool.ThreadSafeConnection;
  stmt := conn.NewStatementPrepared(FORTUNES_SQL, true, true);
  stmt.ExecutePrepared;
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

function TRawAsyncServer.updates(ctxt: THttpServerRequestAbstract): cardinal;
var
  cnt, i: PtrInt;
  res: TWorlds;
  w: TOrmWorld;
  b: TRestBatch;
begin
  result := HTTP_SERVERERROR;
  cnt := getQueriesParamValue(ctxt);
  SetLength(res, cnt);
  b := TRestBatch.Create(fStore.ORM, TOrmWorld, {transrows=}0,
    [boExtendedJson, boNoModelEncoding, boPutNoCacheFlush]);
  w := TOrmWorld.Create;
  try
    for i := 0 to cnt - 1 do
    begin
      if not fStore.Orm.Retrieve(RandomWorld, w) then
        exit;
      w.RandomNumber := RandomWorld;
      b.Update(w);
      res[i].id := w.IDValue;
      res[i].randomNumber := w.RandomNumber;
    end;
    result := fStore.Orm.BatchSend(b);
  finally
    w.Free;
    b.Free;
  end;
  if result <> HTTP_SUCCESS then
    exit;
  ctxt.SetOutJson(SaveJson(res, TypeInfo(TWorlds)));
end;

function TRawAsyncServer.rawupdates(ctxt: THttpServerRequestAbstract): cardinal;
var
  cnt, i: PtrInt;
  words: TWorlds;
  ids, nums: TInt64DynArray;
  conn: TSqlDBConnection;
  stmt: ISQLDBStatement;
begin
  result := HTTP_SERVERERROR;
  conn := fDbPool.ThreadSafeConnection;
  cnt := getQueriesParamValue(ctxt);
  if not getRawRandomWorlds(cnt, words) then
    exit;
  setLength(ids{%H-}, cnt);
  setLength(nums{%H-}, cnt);
  // generate new randoms, fill parameters arrays for update
  for i := 0 to cnt - 1 do
  begin
    words[i].randomNumber := RandomWorld;
    ids[i] := words[i].id;
    nums[i] := words[i].randomNumber;
  end;
  stmt := conn.NewStatementPrepared(WORLD_UPDATE_SQLN, false, true);
  stmt.BindArray(1, ids);
  stmt.BindArray(2, nums);
  stmt.ExecutePrepared;
  //conn.Commit;
  ctxt.SetOutJson(SaveJson(words, TypeInfo(TWorlds)));
  result := HTTP_SUCCESS;
end;



var
  rawServers: array of TRawAsyncServer;
  threads, cores, servers, i: integer;
  flags: THttpServerOptions;

begin
  {$ifdef WITH_LOGS}
  TSynLog.Family.Level := LOG_VERBOSE; // disable logs for benchmarking
  TSynLog.Family.HighResolutionTimestamp := true;
  TSynLog.Family.AutoFlushTimeOut := 1;
  {$endif WITH_LOGS}
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

  Rtti.RegisterFromText([
    TypeInfo(TMessageRec), 'message:RawUtf8',
    TypeInfo(TWorldRec),   'id,randomNumber:integer',
    TypeInfo(TFortune),    'id:integer message:RawUtf8']);

  flags := [];
  if ParamCount > 1 then
  begin
    // user specified some values at command line
    if not TryStrToInt(ParamStr(1), threads) then
      threads := SystemInfo.dwNumberOfProcessors * 4;
    if threads < 2 then
      threads := 2
    else if threads > 256 then
      threads := 256; // max. threads for THttpAsyncServer

    if not TryStrToInt(ParamStr(2), cores) then
      cores := 16;
    if SystemInfo.dwNumberOfProcessors > cores then
      SystemInfo.dwNumberOfProcessors := cores; //for hsoThreadCpuAffinity

    if not TryStrToInt(ParamStr(3), servers) then
      servers := 1;
    if servers < 1 then
      servers := 1
    else if servers > 16 then
      servers := 16;
  end
  else
  begin
    // automatically sets best parameters depending on available CPU cores
    cores := SystemInfo.dwNumberOfProcessors;
    if cores > 12 then
    begin
      // hi-end CPU - scale using several listeners bound to the HW cores
      threads := cores;
      if cores div 4 > 6 then
        servers := 6
      else
        servers := cores div 4;
    end
    else
    begin
      threads := cores * 4;
      servers := 1;
    end;
  end;
  if servers = 1 then
    include(flags, hsoThreadSmooting); // 30% better /plaintext e.g. on i5 7300U

  // start the server instance(s), in hsoReusePort mode
  SetLength(rawServers, servers);
  for i := 0 to servers - 1 do
    rawServers[i] := TRawAsyncServer.Create(threads, flags);
  try
    {$I-}
    writeln;
    writeln(rawServers[0].fHttpServer.ClassName,
     ' running on localhost:', rawServers[0].fHttpServer.SockPort);
    writeln(' num thread=', threads,
            ', num CPU=', SystemInfo.dwNumberOfProcessors,
            ', num servers=', servers,
            ', total workers=', threads * servers,
            ', db=', rawServers[0].fDbPool.DbmsEngineName);
    writeln('Press Ctrl+C or use SIGTERM to terminate'#10);
    FpPause; // mandatory for the actual benchmark tool
    //TSynLog.Family.Level := LOG_VERBOSE; // enable shutdown logs for debug
    for i := 0 to servers - 1 do
      writeln(ObjectToJsonDebug(rawServers[i].fHttpServer,
        [woDontStoreVoid, woHumanReadable]));
  finally
     for i := 0 to servers - 1 do
      rawServers[i].Free;
  end;

  {$ifdef FPC_X64MM}
  WriteHeapStatus(' ', 16, 8, {compileflags=}true);
  {$endif FPC_X64MM}
end.

