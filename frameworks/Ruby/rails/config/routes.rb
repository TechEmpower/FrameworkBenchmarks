Rails.application.routes.draw do
  # For details on the DSL available within this file, see http://guides.rubyonrails.org/routing.html

  JsonApp = if defined?(Falcon) || defined?(Puma) || defined?(Agoo)
    ->(env) do
      [200,
       {
         'Server' => 'Rails',
         'Content-Type' => 'application/json',
         'Date' => Time.now.httpdate,
       },
       [{ 'message' => 'Hello, World!' }.to_json]]
    end
  else
    ->(env) do
      [200,
       {
         'Server' => 'Rails',
         'Content-Type' => 'application/json'
       },
       [{ 'message' => 'Hello, World!' }.to_json]]
    end
  end

  PlaintextApp = if defined?(Falcon) || defined?(Puma) || defined?(Agoo)
    ->(env) do
      [200,
       {
         'Server' => 'Rails',
         'Content-Type' => 'text/plain',
         'Date' => Time.now.httpdate
       },
       ['Hello, World!']]
    end
  else
    ->(env) do
      [200,
       {
         'Server' => 'Rails',
         'Content-Type' => 'text/plain'
       },
       ['Hello, World!']]
    end
  end

  get "json", to: JsonApp
  get "db", to: "hello_world#db"
  get "queries", to: "hello_world#query"
  get "fortunes", to: "fortunes#index"
  get "updates", to: "hello_world#update"
  get "plaintext", to: PlaintextApp
  get "cached", to: "hello_world#cached_query"
end
