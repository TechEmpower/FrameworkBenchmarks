class ApplicationController < ActionController::Base

  before_action :add_header

protected

  def add_header
    response.set_header('Date', Time.now.httpdate)
  end

end
