require 'json'

app = lambda do |env| 
  [
    200,
    { 'Content-Type' => 'application/json' },
   {:message => "Hello World!"}.to_json
  ]
end 
run app 
