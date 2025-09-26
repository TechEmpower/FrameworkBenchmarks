class JsonController < ApplicationControllerMetal
  def index
    add_headers
    self.content_type = 'application/json'
    self.response_body = Oj.dump({ 'message' => 'Hello, World!' })
  end
end
