local json = require "json"
local database = require "database"
local exit = require "exit"


local function process (db)
	local op = db:operators()
	
	return db:findOne({World = {id = op.equal(math.random(1,10000))}})
end


local status, db = pcall(database.connect)
if not status then exit(db) end

local status, res = pcall(process, db)
db:close()

if status then
	ngx.header.content_type = 'application/json'
	ngx.print(json.encode(res))
else
	exit(res)
end