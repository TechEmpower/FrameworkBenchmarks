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
	-- May seem like a stupid branch, but since we know that
	-- at a benchmark it will always be taken one way,
	-- it doesn't matter. For me, after a small warmup, the performance
	-- is identical to a version without the branch
	-- http://wiki.luajit.org/Numerical-Computing-Performance-Guide
	if num_queries == 1 then
		ngx.print(encode(db:query('SELECT * FROM World WHERE id = '..random(1,10000))[1]))
	else
		local worlds = {}
		for i=1, num_queries do
			worlds[#worlds+1] = db:query('SELECT * FROM World WHERE id = '..random(1,10000))[1]
		end
		ngx.print( encode(worlds) )
	end
	db:set_keepalive(0, 256)
end
