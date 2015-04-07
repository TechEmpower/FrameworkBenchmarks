routers = dict(
    BASE = dict(
        default_application='app',
    )
)

routes_in = (
	("/json", "/app/default/json"),
	("/plaintext", "/app/default/plaintext"),
        ("/db", "/app/default/db"),
	("/queries", "/app/default/queries"),
	("/updates", "/app/default/updates"),
	("/fortune", "/app/default/fortune")
)
