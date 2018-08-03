module Web::Controllers::HelloWorld
  class Json
    include Web::Action

    def call(params)
      self.headers.merge!({ 'Content-Type' => 'application/json' })
      status 200, {:message => "Hello, World!"}.to_json
    end
  end
end
