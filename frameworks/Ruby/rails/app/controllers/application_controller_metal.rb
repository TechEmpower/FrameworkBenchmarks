# frozen_string_literal: true
class ApplicationControllerMetal < ActionController::Metal
  def add_headers
    response.set_header('Server', 'rails')
    response.set_header('Date', Time.now.httpdate)
  end
end
