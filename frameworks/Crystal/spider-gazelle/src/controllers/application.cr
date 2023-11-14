# Require kilt for template support
require "kilt"

abstract class Application < ActionController::Base
  before_action :set_date_header

  def set_date_header
    response.headers["Server"] = "Spider-Gazelle"
    response.headers["Date"] = HTTP.format_time(Time.local)
  end
end
