-module(chicagoboss_world_controller, [Req]).
-compile(export_all).

plaintext('GET', []) ->
	{output, "Hello, world!", [{"Content-Type", "text/plain"}]}.

json('GET', []) ->
	{json, [{message, "Hello, world!"}]}.
