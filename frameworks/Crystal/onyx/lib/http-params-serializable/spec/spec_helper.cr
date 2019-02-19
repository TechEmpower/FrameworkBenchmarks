require "spec"
require "uri"

require "../src/http-params-serializable"

macro assert_raise(object, query, error, message, path)
  expect_raises {{error}} do
    begin
      {{object}}.from_query({{query}})
    rescue ex : {{error}}
      ex.message.should eq {{message}}
      ex.path.should eq {{path}}
      raise ex
    end
  end
end

def escape(string : String)
  URI.escape(string) do |byte|
    URI.unreserved?(byte) || byte.chr == '=' || byte.chr == '&'
  end
end
