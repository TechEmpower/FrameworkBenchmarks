# Configure your routes here
# See: http://hanamirb.org/guides/routing/overview/
#
# Example:
# get '/hello', to: ->(env) { [200, {}, ['Hello from Hanami!']] }
get '/plaintext', to: 'hello_world#plaintext'
get '/fortune',  to: 'hello_world#fortune'
get '/update', to: 'hello_world#update'

namespace 'hello_world' do
  get '/json',    to: 'hello_world#json'
  get '/db',      to: 'hello_world#db'
  get '/query', to: 'hello_world#query'
end
