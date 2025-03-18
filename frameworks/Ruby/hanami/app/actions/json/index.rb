# frozen_string_literal: true

module HelloWorld
  module Actions
    module JSON
      class Index < HelloWorld::Action
        def handle(*, response)
          response.format = :json
          response.body = { 'message' => 'Hello, World!' }.to_json
        end
      end
    end
  end
end
