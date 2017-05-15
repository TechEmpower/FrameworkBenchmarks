module Web::Controllers::HelloWorld
  class Json
    include Web::Action

    def call(params)
      status 200, {:message => "Hello, World!"}.to_json
    end
  end
end
