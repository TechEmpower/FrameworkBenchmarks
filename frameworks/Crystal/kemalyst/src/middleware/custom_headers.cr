module Middleware 
  class CustomHeaders < Kemalyst::Handler::Base
    def self.instance
      @@instance ||= new
    end

    def call(context)
      context.response.headers["Server"] = "Kemalyst"
      context.response.headers["Date"] = Time.now.to_s
      call_next(context)
    end
  end
end
