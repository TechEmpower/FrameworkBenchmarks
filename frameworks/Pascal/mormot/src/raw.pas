program raw;

{
 ----------------------------------------------------
   TechEmpower Framework Benchmarks implementation
   in modern pascal and the mORMot 2 framework
 ----------------------------------------------------
 https://github.com/TechEmpower/FrameworkBenchmarks/wiki
 command line optional syntax: raw [threads] [cores] [servers]
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
  private
    fHttpServer: THttpAsyncServer;
    fDbPool: TSqlDBConnectionProperties;
    fModel: TOrmModel;
    fStore: TRestServerDB;
    fTemplate: TSynMustache;
  protected
    // as used by /rawqueries and /rawupdates
    function GetRawRandomWorlds(cnt: PtrInt; out res: TWorlds): boolean;
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
  end;

{$I-}

const
  HELLO_WORLD: RawUtf8 = 'Hello, World!';
  TEXT_CONTENT_TYPE_NO_ENCODING: RawUtf8 = 'text/plain';

  WORLD_COUNT       = 10000;
  WORLD_READ_SQL    = 'select id,randomNumber from World where id=?';
  WORLD_UPDATE_SQLN ='update World as t set randomNumber = v.r from ' +
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
  if fStore.Server.Cache.SetCache(TOrmCachedWorld) then
    fStore.Server.Cache.FillFromQuery(TOrmCachedWorld, '', []);
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
  stmt: ISQLDBStatement;
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
  pConn.Flush; // in case we use modified libpq what not flush inside PQPipelineSync - flush manually
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
  cache: POrmCacheTable;
begin
  cache := fStore.Orm.Cache.Table(TOrmCachedWorld);
  SetLength(res, GetQueriesParamValue(ctxt, 'COUNT='));
  for i := 0 to length(res) - 1 do
    res[i] := cache.Get(ComputeRandomWorld);
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
  stmt: ISQLDBStatement;
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

function FortuneCompareByMessage(const A, B): integer;
begin
  result := StrComp(pointer(TFortune(A).message), pointer(TFortune(B).message));
end;

function TRawAsyncServer.rawfortunes(ctxt: THttpServerRequest): cardinal;
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
  stmt: ISQLDBStatement;
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



var
  rawServers: array of TRawAsyncServer;
  threads, servers, i, k, cpuIdx: integer;
  pinServers2Cores: boolean;
  cpuMask: TCpuSet;
  accessibleCPUCount: PtrInt;
  flags: THttpServerOptions;

function FindCmdLineSwitchVal(const Switch: string; out Value: string): Boolean;
var
  I, L: integer;
  S, T: string;
begin
  Result := False;
  S := Switch;
  Value := '';
  S := UpperCase(S);
  I := ParamCount;
  while (Not Result) and (I>0) do
  begin
    L := Length(Paramstr(I));
    if (L>0) and (ParamStr(I)[1] in SwitchChars) then
    begin
      T := Copy(ParamStr(I),2,L-1);
      T := UpperCase(T);
      Result := S=T;
      if Result and (I <> ParamCount) then
        Value := ParamStr(I+1)
    end;
    Dec(i);
  end;
end;

procedure ComputeExecutionContextFromParams(cpusAccessible: PtrInt);
var
  sw: string;
begin
  // user specified some values at command line: raw [-s serversCount] [-t threadsPerServer] [-p]
  if not FindCmdLineSwitchVal('t', sw) or not TryStrToInt(sw, threads) then
    threads := cpusAccessible * 4;
  if not FindCmdLineSwitchVal('s', sw) or not TryStrToInt(sw, servers) then
    servers := 1;
  pinServers2Cores := FindCmdLineSwitch('p', true) or FindCmdLineSwitch('-pin', true);
  if threads < 1 then
    threads := 1
  else if threads > 256 then
    threads := 256; // max. threads for THttpAsyncServer

  if servers < 1 then
    servers := 1
  else if servers > 256 then
    servers := 256;
end;

procedure ComputeExecutionContextFromNumberOfProcessors(cpusAccessible: PtrInt);
begin
  // automatically guess best parameters depending on available CPU cores
  if cpusAccessible >= 6 then
  begin
    // scale using several listeners (one per core)
    // see https://synopse.info/forum/viewtopic.php?pid=39263#p39263
    servers := cpusAccessible;
    threads := 8;
    pinServers2Cores := true;
  end
  else
  begin
    // low-level CPU - a single instance and a few threads per core
    servers := 1;
    threads := cpusAccessible * 4;
    pinServers2Cores := false;
  end;
end;

begin
  if FindCmdLineSwitch('?') or FindCmdLineSwitch('h') or FindCmdLineSwitch('-help', ['-'], false) then
  begin
    writeln('Usage: ' + UTF8ToString(ExeVersion.ProgramName) + ' [-s serversCount] [-t threadsPerServer] [-p]');
    writeln('Options:');
    writeln('  -?, --help            displays this message');
    writeln('  -s  serversCount      count of servers (listener sockets)');
    writeln('  -t  threadsPerServer  per-server thread poll size');
    writeln('  -p, --pin             pin each server to CPU starting from 0');
    exit;
  end;

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

  // setup execution context
  accessibleCPUCount := CurrentCpuSet(cpuMask);

  if ParamCount > 0 then
    ComputeExecutionContextFromParams(accessibleCPUCount)
  else
    ComputeExecutionContextFromNumberOfProcessors(accessibleCPUCount);
  flags := [];
  if servers > 1 then
    include(flags, hsoReusePort); // allow several bindings on the same port

  // start the server instance(s), in hsoReusePort mode if needed
  SetLength(rawServers{%H-}, servers);
  cpuIdx := -1; // do not pin to CPU by default
  for i := 0 to servers - 1 do begin
    if pinServers2Cores then
    begin
      k := i mod accessibleCPUCount;
      cpuIdx := -1;
      // find real CPU index according to the cpuMask
      repeat
        inc(cpuIdx);
        if GetBit(cpuMask, cpuIdx) then
          dec(k);
      until k = -1;
      writeln('Pin server #', i, ' to #', cpuIdx, ' CPU');
    end;
    rawServers[i] := TRawAsyncServer.Create(threads, flags, cpuIdx)
  end;

  try
    // display some information and wait for SIGTERM
    writeln;
    writeln(rawServers[0].fHttpServer.ClassName,
     ' running on localhost:', rawServers[0].fHttpServer.SockPort);
    writeln(' num thread=', threads,
            ', total CPU=', SystemInfo.dwNumberOfProcessors,
            ', accessible CPU=', accessibleCPUCount,
            ', num servers=', servers,
            ', pinned=', pinServers2Cores,
            ', total workers=', threads * servers,
            ', db=', rawServers[0].fDbPool.DbmsEngineName);
    writeln(' options=', GetSetName(TypeInfo(THttpServerOptions), flags));
    writeln('Press [Enter] or Ctrl+C or send SIGTERM to terminate'#10);
    ConsoleWaitForEnterKey;
    //TSynLog.Family.Level := LOG_VERBOSE; // enable shutdown logs for debug
    if servers = 1 then
      writeln(ObjectToJsonDebug(rawServers[i].fHttpServer,
        [woDontStoreVoid, woHumanReadable]))
    else
    begin
      writeln('Per-server accepted connections:');
      for i := 0 to servers - 1 do
        write(rawServers[i].fHttpServer.Async.Accepted, ' ');
      writeln;
    end;
  finally
    // clear all server instance(s)
    ObjArrayClear(rawServers);
  end;
  {$ifdef FPC_X64MM}
  WriteHeapStatus(' ', 16, 8, {compileflags=}true);
  {$endif FPC_X64MM}
end.
