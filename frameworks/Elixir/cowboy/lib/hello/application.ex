defmodule Hello.Application do
  @moduledoc false

  use Application
  require Logger

  def start(_type, _args) do
    _log_level = Application.get_env(:logger, :level)
    http_ip = Application.get_env(:hello, :http_ip)
    http_port = Application.get_env(:hello, :http_port)
    compress_body = Application.get_env(:hello, :compress_body)
    num_acceptors = Application.get_env(:hello, :num_acceptors)
    max_connections = Application.get_env(:hello, :max_connections)
    max_keepalive = Application.get_env(:hello, :max_keepalive)

    children = [
      {Plug.Cowboy,
       scheme: :http,
       plug: Hello.Router,
       options: [
         ip: http_ip,
         port: http_port,
         compress: compress_body,
         transport_options: [
           num_acceptors: num_acceptors,
           max_connections: max_connections
         ],
         protocol_options: [
           max_keepalive: max_keepalive
         ]
       ]}
    ]

    opts = [strategy: :one_for_one, name: Hello.Supervisor]
    ip_str = http_ip |> Tuple.to_list() |> Enum.join(".")
    Logger.info("Starting up on #{ip_str}:#{http_port}")
    Supervisor.start_link(children, opts)
  end
end
