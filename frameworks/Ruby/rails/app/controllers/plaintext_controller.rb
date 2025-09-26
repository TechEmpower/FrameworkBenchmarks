class PlaintextController < ApplicationControllerMetal
  def index
    add_headers
    self.content_type = 'text/plain'
    self.response_body = 'Hello, World!'
  end
end
