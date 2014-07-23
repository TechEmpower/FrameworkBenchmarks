-module(web_handler).
-export([start/1, run/1, out/1]).

-include_lib("yaws/include/yaws_api.hrl").

start(Options) ->
    {ok, spawn(?MODULE, run, [Options])}.

run(Options) ->
    Id = "embedded",
    GconfList = [{id, Id}],
    SconfList = [{appmods, [{"/", ?MODULE}]},
                 {flags, [{auth_log, false}, {access_log, false}]} | Options],
    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf("priv/www", SconfList, GconfList, Id),
    [supervisor:start_child(yaws_bench_sup, Child) || Child <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    {ok, self()}.

out(Req) ->
    Url = yaws_api:request_url(Req),
    Method = (Req#arg.req)#http_request.method,
    Path = string:tokens(Url#url.path, "/"),
    handle(Method, Path, Req).

%% handle

handle('GET', ["json"], _Req) ->
    json(erl_bench:hello_json());

handle('GET', ["plaintext"], _Req) ->
    plain(erl_bench:hello_plain());

handle('GET', ["db"], _Req) ->
    json(erl_bench:random_json());

handle('GET', ["queries"], Req) ->
    Queries = queries(Req),
    json(erl_bench:randoms_json(Queries));

handle('GET', ["updates"], Req) ->
    Queries = queries(Req),
    json(erl_bench:update_randoms_json(Queries));

handle('GET', ["fortunes"], _Req) ->
    html(erl_bench:fortunes_html());

handle(_Method, _Path, _Req) ->
    [{status, 404}, {content, "text/plain", "Not Found"}].

%% private

ok(Type, Body) ->
    [{status, 200},
     {content, Type, Body}].

json(Json) ->
    ok("application/json", Json).

plain(Text) ->
    ok("text/plain", Text).

html(Html) ->
    ok("text/html", Html).

queries(Req) ->
    Params = yaws_api:parse_query(Req),
    Queries = (catch list_to_integer(proplists:get_value("queries", Params, "1"))),
    case {is_number(Queries), Queries > 500} of
        {true, true} -> 500;
        {false, _}   -> 1;
        _ -> Queries
    end.
