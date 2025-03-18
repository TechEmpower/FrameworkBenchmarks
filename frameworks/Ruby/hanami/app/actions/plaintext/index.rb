# frozen_string_literal: true

module HelloWorld
  module Actions
    module Plaintext
      class Index < HelloWorld::Action
        def handle(*, response)
          response.body = 'Hello, World!'
        end
      end
    end
  end
end
