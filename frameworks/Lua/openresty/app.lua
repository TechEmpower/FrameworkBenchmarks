local mysql = mysql

local encode = encode
local random = math.random
local min = math.min
local insert = table.insert
local sort = table.sort
local template = require'resty.template'
template.caching(false)
-- Compile template, disable cache, enable plain text view to skip filesystem loading
local view = template.compile([[<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>{% for _,f in ipairs(fortunes) do %}<tr><td>{{ f.id }}</td><td>{{ f.message }}</td></tr>{% end %}</table></body></html>]], nil, true)

local mysqlconn = {
	host = "DBHOSTNAME",
	port = 3306,
	database = "hello_world",
	user = "benchmarkdbuser",
	password = "benchmarkdbpass"
}
return {
    db = function(ngx)
        local db = mysql:new()
        assert(db:connect(mysqlconn))
        ngx.print(encode(db:query('SELECT * FROM World WHERE id = '..random(1,10000))[1]))
        db:set_keepalive(0, 256)
    end,
    queries = function(ngx)
        local db = mysql:new()
        assert(db:connect(mysqlconn))
        local num_queries = tonumber(ngx.var.arg_queries) or 1
        -- May seem like a stupid branch, but since we know that
        -- at a benchmark it will always be taken one way,
        -- it doesn't matter. For me, after a small warmup, the performance
        -- is identical to a version without the branch
        -- http://wiki.luajit.org/Numerical-Computing-Performance-Guide
        if num_queries < 2 then
            ngx.print(encode({db:query('SELECT * FROM World WHERE id = '..random(1,10000))[1]}))
        else
            local worlds = {}
            num_queries = min(500, num_queries)
            for i=1, num_queries do
                worlds[#worlds+1] = db:query('SELECT * FROM World WHERE id = '..random(1,10000))[1]
            end
            ngx.print( encode(worlds) )
        end
        db:set_keepalive(0, 256)
    end,
    fortunes = function(ngx)
        local db = mysql:new()
        assert(db:connect(mysqlconn))
        local fortunes = db:query('SELECT * FROM Fortune')
        insert(fortunes, {
            id = 0,
            message = "Additional fortune added at request time."
        })
        sort(fortunes, function(a, b)
            return a.message < b.message
        end)
        local res = view{fortunes=fortunes}
        ngx.header['Content-Length'] = #res
        ngx.print(res)
        db:set_keepalive(0, 256)
    end
}
