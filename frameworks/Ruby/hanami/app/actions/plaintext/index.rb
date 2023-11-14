# frozen_string_literal: true

module HelloWorld
  module Actions
    module Plaintext
      class Index < HelloWorld::Action
        def handle(*, response)
          response.headers['Server'] = 'hanami'
          response.headers['Date'] = Time.now.httpdate
          response.body = 'Hello, World!'
        end
      end
    end
  end
end
