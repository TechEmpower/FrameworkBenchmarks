require 'json'

app = lambda do |env| 
  if env['PATH_INFO'] == "/plaintext"
    [
      200,
      { 'Content-Type' => 'text/plain' },
      ["Hello, World!"]
    ]
  else
    [
      200,
      { 'Content-Type' => 'application/json' },
      [{:message => "Hello, World!"}.to_json]
    ]
  end
end 
run app 

