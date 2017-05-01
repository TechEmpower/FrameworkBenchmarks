local json = require "json"
local database = require "database"
local param = require "param"
local exit = require "exit"


local function process (db)
	local op = db:operators()
	
	local num_queries = tonumber(param.queries) or 1
	if num_queries < 1 then 
		num_queries = 1 
	elseif num_queries > 500 then
		num_queries = 500
	end
	
	local worlds = {}
	for i=1, num_queries do
		local world = db:findOne({World = {id = op.equal(math.random(1,10000))}})
		world.randomNumber = math.random(1,10000)
		db:update({World = world})
		worlds[#worlds + 1] = world
	end
	return worlds
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