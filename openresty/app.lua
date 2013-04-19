local _M = {}

local cjson = require "cjson"
local mysql = require "resty.mysql"
local math = require "math"

local encode = cjson.encode
local random = math.random
local insert = table.insert

local mysqlconn = {
    host = "DBHOSTNAME",
    port = 3306,
    database = "hello_world",
    user = "benchmarkdbuser",
    password = "benchmarkdbpass"
}

function _M.handler(ngx)
    ngx.header.content_type = 'application/json'
    
    if ngx.var.uri == '/json' then
	local resp = {message = "Hello, World!"}
	ngx.print( encode(resp) )
    elseif ngx.var.uri == '/db' then
	local db, err = mysql:new()
	local ok, err = db:connect(mysqlconn)
	local num_queries = tonumber(ngx.var.arg_queries) or 1
	local worlds = {}
	for i=1, num_queries do
	    local wid = random(1, 10000)
	    insert(worlds, db:query('SELECT * FROM World WHERE id = '..wid)[1])
	end
	ngx.print( encode(worlds) )
	local ok, err = db:set_keepalive(0, 256)
    end
end

return _M