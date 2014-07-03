local http = require('leda.http')

local server = http.Server(8080, '')

server:request(function(request, response)
    if request:url():find('/json') then
        response.body =  {message= 'Hello, World!'}
    end
    
    if request:url():find('/plaintext') then
        response.body =  'Hello, World!'
    end
    
end)
