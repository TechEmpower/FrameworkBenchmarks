local mysql = mysql

local encode = encode
local random = math.random

local mysqlconn = {
	host = "DBHOSTNAME",
	port = 3306,
	database = "hello_world",
	user = "benchmarkdbuser",
	password = "benchmarkdbpass"
}
return function(ngx)
	local db = mysql:new()
	assert(db:connect(mysqlconn))
	local num_queries = tonumber(ngx.var.arg_queries) or 1
	local worlds = {}
	for i=1, num_queries do
		local wid = random(1, 10000)
		worlds[#worlds+1] = db:query('SELECT * FROM World WHERE id = '..wid)[1]
	end
	ngx.print( encode(worlds) )
	db:set_keepalive(0, 256)
end
