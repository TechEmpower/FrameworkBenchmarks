Rails.application.routes.draw do
  # For details on the DSL available within this file, see http://guides.rubyonrails.org/routing.html

  get "hello_world/json"
  get "hello_world/db"
  get "hello_world/query"
  get "fortune", to: "hello_world#fortune"
  get "update", to: "hello_world#update"
  get "plaintext", to: "hello_world#plaintext"
end
