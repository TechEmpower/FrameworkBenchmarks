require "../spec_helper"
require "../../src/onyx/rest"

struct TestView
  include Onyx::REST::View

  def initialize(@foo : String)
  end

  text("foo = #{@foo}")
end

struct TestAction
  include Onyx::REST::Action

  params do
    form do
      type foo : String
    end
  end

  def call
    TestView.new(params.form.not_nil!.foo)
  end
end

Onyx.get "/" do |env|
  env.response << "Hello Onyx"
end

Onyx.render(:text)

Onyx.post "/action", TestAction

spawn do
  Onyx.listen(port: 4849)
end

sleep(0.1)

describe "onyx/rest" do
  client = HTTP::Client.new(URI.parse("http://localhost:4849"))

  it do
    response = client.get "/"
    response.status_code.should eq 200
    response.body.should eq "Hello Onyx"
  end

  it do
    response = client.post("/action", headers: HTTP::Headers{"Content-Type" => "application/x-www-form-urlencoded"}, body: "foo=bar")
    response.status_code.should eq 200
    response.body.should eq "foo = bar"
  end
end
