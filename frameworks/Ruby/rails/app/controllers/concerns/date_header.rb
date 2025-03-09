module DateHeader
  extend ActiveSupport::Concern

  included do
    if defined?(Agoo) || defined?(Falcon) || defined?(Puma)
      before_action :add_header
    end
  end

  private

  def add_header
    response.set_header('Date', Time.now.httpdate)
  end
end
