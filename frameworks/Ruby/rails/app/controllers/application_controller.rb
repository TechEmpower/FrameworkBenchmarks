# frozen_string_literal: true
class ApplicationController < ActionController::Base
  before_action :add_header

  private

  def add_header
    response.set_header('Date', Time.now.httpdate)
  end
end
