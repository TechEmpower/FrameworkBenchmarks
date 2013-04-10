local cjson = require "cjson"
local mysql = require "resty.mysql"
local math = require "math"

local mysqlconn = {
    host = "DBHOSTNAME",
    port = 3306,
    database = "hello_world",
    user = "benchmarkdbuser",
    password = "benchmarkdbpass"
}

ngx.header.content_type = 'application/json'

if ngx.var.uri == '/json' then
    local resp = {message = "Hello, World!"}
    ngx.print( cjson.encode(resp) )

elseif ngx.var.uri == '/db' then
    local db, err = mysql:new()
    local ok, err = db:connect(mysqlconn)
    local num_queries = tonumber(ngx.req.get_uri_args()["queries"]) or 1
    local worlds = {}
    for i=1, num_queries do
        local wid = math.random(1, 10000)
        table.insert(worlds, db:query('SELECT * FROM World WHERE id = '..wid)[1])
    end
    ngx.print( cjson.encode(worlds) )
    local ok, err = db:set_keepalive(0, 256)
end
