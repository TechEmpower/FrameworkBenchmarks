# frozen_string_literal: true

class ApplicationController < ActionController::Base
  if defined?(Agoo) || defined?(Falcon) || defined?(Puma)
    before_action :add_date_header
  end

  private

  def add_date_header
    response.set_header('Date', Time.now.httpdate)
  end
end
