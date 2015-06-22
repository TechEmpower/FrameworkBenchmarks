defmodule Hello do

  def start(_type, _args) do
    dispatch = :cowboy_router.compile([

      { :_,
        [
          {"/json", JsonHandler, []},
          {"/plaintext", PlaintextHandler, []}
      ]}
    ])
    { :ok, _ } = :cowboy.start_http(:http,
                                    5000,
                                   [{:port, 8080}],
                                   [{ :env, [{:dispatch, dispatch}]}]
                                   )
  end
end

defmodule JsonHandler do
  def init(_type, req, []) do
    {:ok, req, :no_state}
  end

  def handle(request, state) do
    Poison.encode!(%{message: "Hello, World!"})
    { :ok, reply } = :cowboy_req.reply(200,
      [{"content-type", "application/json"}],
      Poison.encode!(%{:message => "Hello, World!"}),
      request)
    { :ok, reply, state }
  end

  def terminate(reason, request, state) do
    :ok
  end
end

defmodule PlaintextHandler do
  def init(_type, req, []) do
    {:ok, req, :no_state}
  end

  def handle(request, state) do
    Poison.encode!(%{message: "Hello, World!"})
    { :ok, reply } = :cowboy_req.reply(200,
      [{"content-type", "text/plain"}],
      "Hello, World!",
      request)
    { :ok, reply, state }
  end

  def terminate(reason, request, state) do
    :ok
  end
end
