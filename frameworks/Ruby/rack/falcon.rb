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

# load :rack, :supervisor

# hostname = File.basename(__dir__)
# rack hostname do
#   # TODO: Update proctocol to use HTTP/2
#   protocol { Async::HTTP::Protocol::HTTP11 }
#   endpoint do
#     Async::HTTP::Endpoint.parse("http://#{ENV.fetch("BINDING")}").with(
#       protocol:
#
#     )
#   end
# end

# supervisor

# #!/usr/bin/env -S falcon host
# # frozen_string_literal: true

# require_relative "lib/application"

# load Lively::Environments::Application

# hostname = File.basename(__dir__)
# port = ENV["PORT"] || 3000

# host hostname, :lively do
#   endpoint Async::HTTP::Endpoint.parse("http://0.0.0.0:#{port}").with(
#              protocol: Async::HTTP::Protocol::HTTP11
#            )
# end

# #!/usr/bin/env -S falcon host
# # frozen_string_literal: true

# load :rack

# hostname = File.basename(__dir__)
# port = ENV["PORT"] || 3000

# rack hostname do
#   append preload "preload.rb"

#   cache false
#   verbose true

#   endpoint Async::HTTP::Endpoint.parse("http://0.0.0.0:#{port}").with(
#              protocol: Async::HTTP::Protocol::HTTP11
#            )
# end

# #!/usr/bin/env -S falcon host
# # frozen_string_literal: true

# load :rack, :lets_encrypt_tls, :supervisor

# hostname = File.basename(__dir__)
# rack hostname, :lets_encrypt_tls do
#   cache true
# end

# supervisor

# #!/usr/bin/env -S falcon host
# # frozen_string_literal: true

# load :rack, :supervisor

# hostname = File.basename(__dir__)
# rack hostname do
#   endpoint Async::HTTP::Endpoint.parse("http://localhost:3000").with(
#              protocol: Async::HTTP::Protocol::HTTP2
#            )
# end

# supervisor
