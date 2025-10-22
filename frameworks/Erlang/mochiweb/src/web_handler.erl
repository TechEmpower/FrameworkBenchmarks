-module(web_handler).
-export([start/1, stop/0, dispatch/1]).

start(Options) ->
    mochiweb_http:start([{name, ?MODULE},
                         {loop, {?MODULE, dispatch}} | Options]).

stop() ->
    mochiweb_http:stop(?MODULE).

dispatch(Req) ->
    Method = mochiweb_request:get(method, Req),
    Path = mochiweb_request:get(path, Req),
    handle(Method, Path, Req).

%% handle

handle('GET', "/json", Req) ->
    json(Req, erl_bench:hello_json());

handle('GET', "/plaintext", Req) ->
    plain(Req, erl_bench:hello_plain());

handle('GET', "/db", Req) ->
    Json = erl_bench:random_json(),
    json(Req, Json);

handle('GET', "/queries", Req) ->
    Queries = queries(Req),
    json(Req, erl_bench:randoms_json(Queries));

handle('GET', "/updates", Req) ->
    Queries = queries(Req),
    json(Req, erl_bench:update_randoms_json(Queries));

handle('GET', "/fortunes", Req) ->
    html(Req, erl_bench:fortunes_html());

handle(_Method, _Path, Req) ->
    mochiweb_request:respond({404, [{"Content-Type", "text/plain"}], "404 Not Found"}, Req).

%% private

json(Req, Json) ->
    mochiweb_request:ok({"application/json", jiffy:encode(Json)}, Req).

plain(Req, Text) ->
    mochiweb_request:ok({"text/plain", Text}, Req).

html(Req, Html) ->
    mochiweb_request:ok({"text/html;charset=UTF-8", Html}, Req).

queries(Req) ->
    Params = mochiweb_request:parse_qs(Req),
    Queries = (catch list_to_integer(proplists:get_value("queries", Params, "1"))),
    case {is_number(Queries), Queries > 500} of
        {true, true} -> 500;
        {false, _}   -> 1;
        _ when Queries < 1 -> 1;
        _ -> Queries
    end.
