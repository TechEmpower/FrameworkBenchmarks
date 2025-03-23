# auto_register: false
# frozen_string_literal: true

require "hanami/action"

module HelloWorld
  class Action < Hanami::Action
    before :set_headers

    private

    def set_headers(*, response)
      response.headers['Server'] = 'hanami'
      response.headers['Date'] = Time.now.httpdate
    end
  end
end
