local http = require('leda.http')

local server = http.Server(8080, '')

server.request = function(server, request, response)
    local url = request:url() 
    if url:find('/json') then
        response.body =  {message= 'Hello, World!'}
    elseif url:find('/plaintext') then    
        response.body =  'Hello, World!'
    end
end
