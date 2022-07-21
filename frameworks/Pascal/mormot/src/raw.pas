program raw;

{
TechEmpower framework benchmarks implementation
See https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview
}

{$I mormot.defines.inc}

{.$define USE_SQLITE3}
// may be defined to use a SQLite3 database instead of external PostgreSQL DB

{.$define SHOW_TIME}
// display each request time on the console (slow down the process)

{.$define WITH_LOGS}
// logging is fine for debugging, less for benchmarking ;)

uses
  {$I mormot.uses.inc} // include mormot.core.fpcx64mm
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
  mormot.db.raw.sqlite3,
  mormot.db.raw.sqlite3.static,
  {$ifdef USE_SQLITE3}
  mormot.db.sql.sqlite3,
  {$endif USE_SQLITE3}
  mormot.rest.sqlite3,
  mormot.net.http,
  mormot.net.server,
  mormot.net.async,
  mormot.db.sql,
  mormot.db.sql.postgres;

type
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

  TOrmWorld = class(TOrm)
  protected
    fRandomNumber: integer;
  published
    property RandomNumber: integer
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

  { TRawAsyncServer }

  TRawAsyncServer = class
  private
    fHttpServer: THttpServerSocketGeneric;
    fDbPool: TSqlDBConnectionProperties;
    fModel: TOrmModel;
    fStore: TRestServerDB;
    fTemplate: TSynMustache;
  protected
    // this is where the process would take place
    function DoOnRequest(ctxt: THttpServerRequestAbstract): cardinal;
    /// return ?queries= parameter value. If missed or < 1 return 1, if > 500 return 500
    function getQueriesParamValue(ctxt: THttpServerRequestAbstract;
      const search: RawUtf8 = 'QUERIES='): integer;
    procedure getRandomWorlds(cnt: PtrInt; out res: TWorlds);
    {$ifdef USE_SQLITE3}
    procedure GenerateDB;
    {$endif USE_SQLITE3}
  public
    constructor Create(threadCount: integer);
    destructor Destroy; override;
    // those are the implementation methods
    function json(ctxt: THttpServerRequestAbstract): cardinal;
    function db(ctxt: THttpServerRequestAbstract): cardinal;
    // /queries and /cached-queries endpoints are implemented in doqueries
    function doqueries(ctxt: THttpServerRequestAbstract; orm: TOrmWorldClass;
      const search: RawUtf8): cardinal;
    function fortunes(ctxt: THttpServerRequestAbstract): cardinal;
    function updates(ctxt: THttpServerRequestAbstract): cardinal;
    function plaintext(ctxt: THttpServerRequestAbstract): cardinal;
    function rawdb(ctxt: THttpServerRequestAbstract): cardinal;
    function rawqueries(ctxt: THttpServerRequestAbstract): cardinal;
    function rawfortunes(ctxt: THttpServerRequestAbstract): cardinal;
    function rawupdates(ctxt: THttpServerRequestAbstract): cardinal;
  end;

const
  TEXT_CONTENT_TYPE_NO_ENCODING: RawUtf8 = 'text/plain';
  JSON_CONTENT_TYPE_NO_ENCODING: RawUtf8 = 'application/json';
  HELLO_WORLD: RawUtf8 = 'Hello, World!';
  WORLD_COUNT = 10000;
  WORLD_READ_SQL = 'select id, randomNumber from World where id=?';
  WORLD_UPDATE_SQLN ='update World as t set randomNumber = v.r from ' +
  '(SELECT unnest(?::NUMERIC[]), unnest(?::NUMERIC[])) as v(id, r)' +
  ' where t.id = v.id';

  FORTUNES_SQL = 'select id, message from Fortune';
  FORTUNES_MESSAGE = 'Additional fortune added at request time.';
  FORTUNES_TPL = '<!DOCTYPE html>'#10 +
   '<html>'#10 +
   '<head><title>Fortunes</title></head>'#10 +
   '<body>'#10 +
   '<table>'#10 +
   '<tr><th>id</th><th>message</th></tr>'#10 +
   '{{#list}}'#10 +
   '<tr><td>{{id}}</td><td>{{message}}</td></tr>'#10 +
   '{{/list}}'#10 +
   '</table>'#10 +
   '</body>'#10 +
   '</html>';


{ TRawAsyncServer }

{$I-}

function TRawAsyncServer.DoOnRequest(ctxt: THttpServerRequestAbstract): cardinal;
const
  ROUTES: array[0..11] of PAnsiChar = (
     '/JSON', '/DB', '/QUERIES', '/FORTUNES', '/UPDATES', '/PLAINTEXT', '/CACHED-QUERIES',
     '/RAWDB' , '/RAWQUERIES', '/RAWFORTUNES', '/RAWUPDATES', nil);
var
  routeIdx, routeL: PtrInt;
  {$ifdef SHOW_TIME}
  timer: TPrecisionTimer;
  {$endif SHOW_TIME}
begin
  {$ifdef WITH_LOGS}
  TSynLog.Add.Log(sllServiceCall, 'DoOnRequest % %', [ctxt.Method, ctxt.Url], self);
  {$endif WITH_LOGS}
  result := HTTP_NOTFOUND;
  routeIdx := IdemPPChar(pointer(ctxt.Url), @ROUTES);
  if routeIdx < 0 then
    exit;
  {$ifdef SHOW_TIME}
  timer.Start;
  {$endif SHOW_TIME}
  routeL := StrLen(ROUTES[routeIdx]);
  if ctxt.Url[routeL + 1] in [#0, '?', '/'] then
    case routeIdx of
      0: result := json(ctxt);
      1: result := db(ctxt);
      2: result := doqueries(ctxt, TOrmWorld, 'QUERIES=');
      3: result := fortunes(ctxt);
      4: result := updates(ctxt);
      5: result := plaintext(ctxt);
      6: result := doqueries(ctxt, TOrmCachedWorld, 'COUNT=');
      // raw tests
      7: result := rawdb(ctxt);
      8: result := rawqueries(ctxt);
      9: result := rawfortunes(ctxt);
      10: result := rawupdates(ctxt);
    end;
  {$ifdef SHOW_TIME}
  write(timer.Stop:8);
  {$endif SHOW_TIME}
end;

constructor TRawAsyncServer.Create(threadCount: integer);
begin
  inherited Create;
  {$ifdef USE_SQLITE3}
  fDbPool := TSqlDBSQLite3ConnectionProperties.Create(
      SQLITE_MEMORY_DATABASE_NAME, '', '', '');
  {$else}
  fDbPool := TSqlDBPostgresConnectionProperties.Create('',
    'postgresql://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world',
    'benchmarkdbuser',
    'benchmarkdbpass');
  {$endif USE_SQLITE3}
  fModel := TOrmModel.Create([TOrmWorld, TOrmFortune, TOrmCachedWorld]);
  VirtualTableExternalRegister(fModel, [TOrmWorld, TOrmFortune], fDbPool);
  // CachedWorld table doesn't exists in DB, but should as read in requirements.
  // Use world table as in other implementations.
  VirtualTableExternalRegister(fModel, TOrmCachedWorld, fDbPool, 'world');
  fStore := TRestServerDB.Create(fModel, SQLITE_MEMORY_DATABASE_NAME);
  {$ifdef USE_SQLITE3}
  GenerateDB;
  {$else}
  fStore.Server.CreateMissingTables; // create SQlite3 virtual tables
  {$endif USE_SQLITE3}
  if fStore.Server.Cache.SetCache(TOrmCachedWorld) then
    fStore.Server.Cache.FillFromQuery(TOrmCachedWorld, '', []);
  fTemplate := TSynMustache.Parse(FORTUNES_TPL);
  fHttpServer := THttpAsyncServer.Create(
    '8080', nil, nil, '', threadCount, 30000,
    [hsoNoXPoweredHeader, // not needed for a benchmark
     hsoHeadersInterning, // reduce memory contention for /plaintext and /json
     hsoIncludeDateHeader // required by TPW General Test Requirements #5
     //, hsoLogVerbose
    ]);
  fHttpServer.HttpQueueLength := 100000; // needed e.g. from wrk/ab benchmarks
  fHttpServer.OnRequest := DoOnRequest;
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

function RandomWorld: integer; inline;
begin
  result := Random32(WORLD_COUNT) + 1;
end;

{$ifdef USE_SQLITE3}

const
  _FORTUNES: array[1..12] of RawUtf8 = (
  'fortune: No such file or directory',
  'A computer scientist is someone who fixes things that aren''t broken.',
  'After enough decimal places, nobody gives a damn.',
  'A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1',
  'A computer program does what you tell it to do, not what you want it to do.',
  'Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen',
  'Any program that runs right is obsolete.',
  'A list is only as strong as its weakest link. — Donald Knuth',
  'Feature: A bug with seniority.',
  'Computers make very fast, very accurate mistakes.',
  '<script>alert("This should not be displayed in a browser alert box.");</script>',
  'フレームワークのベンチマーク');

procedure TRawAsyncServer.GenerateDB;
var
  i: PtrInt;
  b: TRestBatch;
  w: TOrmWorld;
  f: TOrmFortune;
begin
  fStore.Server.CreateMissingTables;
  w := TOrmWorld.Create;
  f := TOrmFortune.Create;
  b := TRestBatch.Create(fStore.Orm, nil);
  try
    for i := 1 to WORLD_COUNT do
    begin
      w.IDValue := i;
      w.RandomNumber := RandomWorld;
      b.Add(w, true, true);
    end;
    for i := low(_FORTUNES) to high(_FORTUNES) do
    begin
      f.IDValue := i;
      f.Message := _FORTUNES[i];
      b.Add(f, true, true);
    end;
    if fStore.Orm.BatchSend(b) <> HTTP_SUCCESS then
      raise EOrmBatchException.Create('GenerateDB failed');
  finally
    b.Free;
    f.Free;
    w.Free;
  end;
end;

{$endif USE_SQLITE3}

function TRawAsyncServer.json(ctxt: THttpServerRequestAbstract): cardinal;
var
  msgRec: TMessageRec;
begin
  msgRec.message := HELLO_WORLD;
  ctxt.OutContentType := JSON_CONTENT_TYPE_NO_ENCODING;
  ctxt.OutContent := SaveJson(msgRec, TypeInfo(TMessageRec));
  result := HTTP_SUCCESS;
end;

function TRawAsyncServer.plaintext(ctxt: THttpServerRequestAbstract): cardinal;
begin
  ctxt.OutContentType := TEXT_CONTENT_TYPE_NO_ENCODING;
  ctxt.OutContent := HELLO_WORLD;
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
    ctxt.OutContent := FormatUtf8('{"id":%,"randomNumber":%}',
      [stmt.ColumnInt(0), stmt.ColumnInt(1)]);
    ctxt.OutContentType := JSON_CONTENT_TYPE_NO_ENCODING;
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
    ctxt.OutContent := FormatUtf8('{"id":%,"randomNumber":%}',
      [w.IDValue, w.randomNumber]);
    ctxt.OutContentType := JSON_CONTENT_TYPE_NO_ENCODING;
    result := HTTP_SUCCESS;
  finally
    w.Free;
  end;
end;

function TRawAsyncServer.getQueriesParamValue(ctxt: THttpServerRequestAbstract;
  const search: RawUtf8): integer;
var
  p: PUtf8Char;
begin
  result := 0;
  p := PosChar(pointer(ctxt.Url), '?');
  if p <> nil then
    UrlDecodeInteger(p + 1, search, result);
  if result = 0 then
    result := 1
  else if result > 500 then
    result := 500;
end;

procedure TRawAsyncServer.getRandomWorlds(cnt: PtrInt; out res: TWorlds);
var
  conn: TSqlDBConnection;
  stmt: ISQLDBStatement;
  i: PtrInt;
begin
  SetLength(res{%H-}, cnt);
  conn := fDbPool.ThreadSafeConnection;
  {$ifndef USE_SQLITE3}
  stmt := conn.NewStatementPrepared(WORLD_READ_SQL, true, true);
  {$endif USE_SQLITE3}
  for i := 0 to cnt - 1 do
  begin
    {$ifdef USE_SQLITE3}
    stmt := conn.NewStatementPrepared(WORLD_READ_SQL, true, true);
    {$endif USE_SQLITE3}
    stmt.Bind(1, RandomWorld);
    stmt.ExecutePrepared;
    if not stmt.Step then
      exit;
    res[i].id := stmt.ColumnInt(0);
    res[i].randomNumber := stmt.ColumnInt(1);
    stmt.ReleaseRows;
  end;
end;

function TRawAsyncServer.rawqueries(ctxt: THttpServerRequestAbstract): cardinal;
var
  cnt: PtrInt;
  res: TWorlds;
begin
  cnt := getQueriesParamValue(ctxt);
  getRandomWorlds(cnt, res);
  if length(res) = 0 then
    exit(HTTP_SERVERERROR);

  ctxt.OutContentType := JSON_CONTENT_TYPE_NO_ENCODING;
  ctxt.OutContent := SaveJson(res, TypeInfo(TWorlds));

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
  ctxt.OutContentType := JSON_CONTENT_TYPE_NO_ENCODING;
  ctxt.OutContent := SaveJson(res, TypeInfo(TWorlds));
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
  data: TDocVariantData;
begin
  result := HTTP_SERVERERROR;
  arr.Init(TypeInfo(TOrmFortunes), list);
  if fStore.Orm.RetrieveListObjArray(list, TOrmFortune, '', []) then
    try
      new := TOrmFortune.Create;
      new.Message := FORTUNES_MESSAGE;
      arr.Add(new);
      arr.Sort(OrmFortuneCompareByMessage);
      data.InitArrayFrom(arr, JSON_FAST);
      ctxt.OutContent := fTemplate.Render(_ObjFast(['list', variant(data)]));
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
  f: TFortune;
  arr: TDynArray;
  data: TDocVariantData;
begin
  result := HTTP_SERVERERROR;
  conn := fDbPool.ThreadSafeConnection;
  stmt := conn.NewStatementPrepared(FORTUNES_SQL, true, true);
  stmt.ExecutePrepared;
  arr.Init(TypeInfo(TFortunes), list);
  while stmt.Step do
  begin
    f.id := stmt.ColumnInt(0);
    f.message := stmt.ColumnUtf8(1);
    arr.Add(f);
  end;
  f.id := 0;
  f.message := FORTUNES_MESSAGE;
  arr.Add(f);
  arr.Sort(FortuneCompareByMessage);
  data.InitArrayFrom(arr, JSON_FAST);
  ctxt.OutContent := fTemplate.Render(_ObjFast(['list', variant(data)]));
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
  b := TRestBatch.Create(fStore.ORM, TOrmWorld);
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
  ctxt.OutContentType := JSON_CONTENT_TYPE_NO_ENCODING;
  ctxt.OutContent := SaveJson(res, TypeInfo(TWorlds));
end;

function TRawAsyncServer.rawupdates(ctxt: THttpServerRequestAbstract): cardinal;
var
  cnt, i: PtrInt;
  words: TWorlds;
  ids, nums: TInt64DynArray;
  conn: TSqlDBConnection;
  stmt: ISQLDBStatement;
begin
  cnt := getQueriesParamValue(ctxt);
  getRandomWorlds(cnt, words);

  if length(words) <> cnt then
    exit(HTTP_SERVERERROR);

  setLength(ids, cnt);
  setLength(nums, cnt);

  // generate new randoms, fill parameters arrays for update
  for i := 0 to cnt - 1 do
  begin
    words[i].randomNumber := RandomWorld;
    ids[i] := words[i].id;
    nums[i] := words[i].randomNumber;
  end;
  conn := fDbPool.ThreadSafeConnection;
  conn.StartTransaction;
  stmt := conn.NewStatementPrepared(WORLD_UPDATE_SQLN, false);
  stmt.BindArray(1, nums);
  stmt.BindArray(2, ids);
  stmt.ExecutePrepared;
  conn.Commit;

  ctxt.OutContentType := JSON_CONTENT_TYPE_NO_ENCODING;
  ctxt.OutContent := SaveJson(words, TypeInfo(TWorlds));

  result := HTTP_SUCCESS;
end;



var
  rawServer: TRawAsyncServer;
  threads: integer;

begin
  {$ifdef WITH_LOGS}
  TSynLog.Family.Level := LOG_VERBOSE; // disable logs for benchmarking
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;
  {$endif WITH_LOGS}

  Rtti.RegisterFromText([
    TypeInfo(TMessageRec), 'message:RawUtf8',
    TypeInfo(TWorldRec), 'id,randomNumber:integer',
    TypeInfo(TFortune), 'id:integer message:RawUtf8']);

  if (ParamCount <> 1) or
     not TryStrToInt(ParamStr(1), threads) then
    threads := SystemInfo.dwNumberOfProcessors * 4;
  if threads < 16 then
    threads := 16
  else if threads > 64 then
    threads := 64; // prevents too many PostgreSQL per connection forks
  rawServer := TRawAsyncServer.Create(threads);
  try
    {$I-}
    writeln;
    writeln(rawServer.fHttpServer.ClassName, ' running on localhost:',
      rawServer.fHttpServer.SockPort, '; num thread=', threads, #10);
    {$ifdef USE_SQLITE3}
    writeln('Press [Enter] to terminate'#10);
    readln;
    {$else}
    writeln('Press Ctrl+C or use SIGTERM to terminate'#10);
    FpPause;
    {$endif USE_SQLITE3}
    {$ifdef FPC_X64MM}
    WriteHeapStatus(' ', 16, 8, {compileflags=}true);
    {$endif FPC_X64MM}
  finally
    rawServer.Free;
  end;

end.