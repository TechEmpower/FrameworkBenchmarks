Rails.application.routes.draw do
  # For details on the DSL available within this file, see http://guides.rubyonrails.org/routing.html

  get "json", to: JsonController.action(:index)
  get "db", to: "hello_world#db"
  get "queries", to: "hello_world#query"
  get "fortunes", to: "hello_world#fortune"
  get "updates", to: "hello_world#update"
  get "plaintext", to: PlaintextController.action(:index)
  get "cached", to: "hello_world#cached_query"
end
