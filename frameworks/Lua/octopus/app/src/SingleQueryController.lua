local json = require "json"
local database = require "database"
local exit = require "exit"


local function process (db)
	ngx.header.content_type = 'application/json'
	
	local op = db:operators()
	
	return db:findOne({World = {id = op.equal(math.random(1,10000))}})
end


local db = database.connect()
local status, res = pcall(process, db)
db:close()


if status then
	ngx.print(json.encode(res))
else
	exit(res)
end