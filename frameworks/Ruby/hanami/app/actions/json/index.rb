# frozen_string_literal: true

module HelloWorld
  module Actions
    module JSON
      class Index < HelloWorld::Action
        def handle(*, response)
          response.headers['Server'] = 'hanami'
          response.headers['Date'] = Time.now.httpdate
          response.format = :json
          response.body = { 'message' => 'Hello, World!' }.to_json
        end
      end
    end
  end
end
