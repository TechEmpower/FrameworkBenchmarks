-module(web_handler).
-export([start/1, stop/0, dispatch/1]).

start(Options) ->
    mochiweb_http:start([{name, ?MODULE},
                         {loop, {?MODULE, dispatch}} | Options]).

stop() ->
    mochiweb_http:stop(?MODULE).

dispatch(Req) ->
    Method = Req:get(method),
    Path = string:tokens(Req:get(path), "/"),
    handle(Method, Path, Req).

%% handle

handle('GET', ["json"], Req) ->
    json(Req, erl_bench:hello_json());

handle('GET', ["plaintext"], Req) ->
    plain(Req, erl_bench:hello_plain());

handle('GET', ["db"], Req) ->
    json(Req, erl_bench:random_json());

handle('GET', ["queries"], Req) ->
    Queries = queries(Req),
    json(Req, erl_bench:randoms_json(Queries));

handle('GET', ["updates"], Req) ->
    Queries = queries(Req),
    json(Req, erl_bench:update_randoms_json(Queries));

handle('GET', ["fortunes"], Req) ->
    html(Req, erl_bench:fortunes_html());

handle(_Method, _Path, Req) ->
    Req:respond({404, [{"Content-Type", "text/plain"}], "Not Found"}).

%% private

json(Req, Json) ->
    Req:ok({"application/json", Json}).

plain(Req, Text) ->
    Req:ok({"text/plain", Text}).

html(Req, Html) ->
    Req:ok({"text/html", Html}).

queries(Req) ->
    Params = Req:parse_qs(),
    Queries = (catch list_to_integer(proplists:get_value("queries", Params, "1"))),
    case {is_number(Queries), Queries > 500} of
        {true, true} -> 500;
        {false, _}   -> 1;
        _ -> Queries
    end.
