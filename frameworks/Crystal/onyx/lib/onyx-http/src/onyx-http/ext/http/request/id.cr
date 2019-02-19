require "http/request"

class HTTP::Request
  # A request ID. Can be set by `Onyx::REST::RequestID`.
  property! id : String
end
