local http = require('leda.http')
local json = require('cjson')

local server = http.Server(8080, '')

server.request = function(server, request, response)
    local url = request:url() 
    if url:find('/json') then
        response.body =  json.encode{message= 'Hello, World!'}
        response.headers['Content-Type'] = 'application/json'
    elseif url:find('/plaintext') then    
        response.body =  'Hello, World!'
    end
end
