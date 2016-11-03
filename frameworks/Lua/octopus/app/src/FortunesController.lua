local json = require "json"
local database = require "database"
local exit = require "exit"

local template = require'template'
template.caching(false)
-- Compile template, disable cache, enable plain text view to skip filesystem loading
local view = template.compile([[<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>{% for _,f in ipairs(fortunes) do %}<tr><td>{{ f.id }}</td><td>{{ f.message }}</td></tr>{% end %}</table></body></html>]], nil, true)


local function process (db)
	local fortunes = db:find({Fortune = {}})
	table.insert(fortunes, {id = 0, message = "Additional fortune added at request time."})
	table.sort(fortunes, function(a, b)
		return a.message < b.message
	end)
    return fortunes
end


local status, db = pcall(database.connect)
if not status then exit(db) end

local status, res = pcall(process, db)
db:close()

if status then
	local html = view({fortunes = res})
	ngx.header.content_type = 'text/html; charset=utf-8'
	ngx.header.content_length = #html
	ngx.print(html)
else
	exit(res)
end