defmodule Hello.Endpoint do
  use Phoenix.Endpoint, otp_app: :hello

  plug Plug.Parsers,
    parsers: [:json, :urlencoded, :multipart],
    pass: ["*/*"],
    json_decoder: Jason

  plug Hello.Router
end
