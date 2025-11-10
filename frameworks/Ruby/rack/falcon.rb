#!/usr/bin/env -S falcon host
# frozen_string_literal: true

load :rack, :supervisor

hostname = File.basename(__dir__)
rack hostname do
  endpoint Async::HTTP::Endpoint.parse('http://0.0.0.0:8080').with(
    protocol: Async::HTTP::Protocol::HTTP11
  )
end

supervisor

