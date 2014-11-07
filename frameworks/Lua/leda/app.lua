local http = require('leda.server.http')
local json = require('cjson')

local server = http(8080, 'localhost')

server.request = function(server, request, response)
    local url = request:url() 
    if url:find('/json') then
        response.body =  json.encode{message= 'Hello, World!'}
        response.headers['Content-Type'] = 'application/json'
    elseif url:find('/plaintext') then    
        response.body =  'Hello, World!'
    end
    response:send()
end
