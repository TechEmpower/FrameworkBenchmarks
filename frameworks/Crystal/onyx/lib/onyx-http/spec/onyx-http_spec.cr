require "http/client"
require "./spec_helper"
require "./spec_server"

describe Onyx::HTTP do
  port = 4891
  server = SpecServer.new

  spawn do
    server.server.bind_tcp(port)
    server.server.listen
  end

  sleep 0.1

  client = HTTP::Client.new("localhost", port)

  describe "/" do
    it do
      response = client.get("/")
      response.status_code.should eq 200
      response.body.should eq ""
    end
  end

  describe "/hello" do
    it do
      response = client.get("/hello")
      response.status_code.should eq 200
      response.body.should eq "Hello Onyx"
    end
  end

  describe "/error" do
    it do
      response = client.get("/error")
      response.status_code.should eq 500
      response.body.should eq "500 Internal Server Error"
    end
  end

  describe "/unknown" do
    it do
      response = client.get("/unknown")
      response.status_code.should eq 404
      response.body.should eq "404 Route Not Found: GET /unknown"
    end
  end

  server.server.close
end
