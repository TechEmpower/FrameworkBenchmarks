require "../spec_helper"
require "../../src/onyx-rest/renderers/text"

struct FooAction
  include Onyx::REST::Action

  params do
    path do
      type path_param : Int32
    end

    query do
      type query_param : String?
    end

    form do
      type form_param do
        type foo : Int32
        type bar : String? = "baz"
      end
    end

    json do
      type json_param, key: "jsonParam" do
        type foo : Int32
        type bar : String? = "baz"
      end
    end
  end

  errors do
    type OffendingQuery(401)

    type Teapot(419), flavor : String do
      super("I'm a teapot with #{flavor} tea")
    end
  end

  def call
    case params.query.query_param
    when "bang"           then raise OffendingQuery.new
    when "green", "black" then raise Teapot.new(params.query.query_param.not_nil!)
    else
      if json = params.json
        context.response << "#{params.path.path_param}, #{params.query.query_param}, JSON: #{json.json_param.foo}, #{json.json_param.bar}"
      elsif form = params.form
        context.response << "#{params.path.path_param}, #{params.query.query_param}, form: #{form.form_param.foo}, #{form.form_param.bar}"
      else
        context.response << "#{params.path.path_param}, #{params.query.query_param}"
      end
    end
  end
end

class ActionSpecServer
  getter server

  def initialize
    renderer = Onyx::REST::Renderers::Text.new
    rescuer = Onyx::REST::Rescuer.new(renderer)
    router = Onyx::HTTP::Router.new do
      post "/foo/:path_param", FooAction
    end

    @server = Onyx::HTTP::Server.new([rescuer, router, renderer])
  end
end

describe Onyx::REST::Action do
  server = ActionSpecServer.new

  spawn do
    server.server.bind_tcp(4890)
    server.server.listen
  end

  sleep(0.1)

  client = HTTP::Client.new(URI.parse("http://localhost:4890"))

  describe "params parsing" do
    context "JSON" do
      it do
        response = client.post("/foo/42?query_param=bar&unknown_param=42",
          headers: ::HTTP::Headers{"Content-Type" => "application/x-www-form-urlencoded"},
          body: "formParam[foo]=42&formParam[bar]=qux"
        )
        response.status_code.should eq 200
        response.body.should eq "42, bar, form: 42, qux"
      end
    end

    context "form" do
      it do
        response = client.post("/foo/42?query_param=bar&unknown_param=42",
          headers: ::HTTP::Headers{"Content-Type" => "application/json"},
          body: %Q[{"jsonParam": {"foo": 42, "bar": "qux"}}]
        )
        response.status_code.should eq 200
        response.body.should eq "42, bar, JSON: 42, qux"
      end
    end
  end

  describe "REST errors handling" do
    it do
      response = client.post("/foo/42?query_param=bang")
      response.status_code.should eq 401
      response.body.should eq "401 Offending Query"
    end
    it do
      response = client.post("/foo/42?query_param=green")
      response.status_code.should eq 419
      response.body.should eq "419 I'm a teapot with green tea"
    end
  end

  server.server.close
end
