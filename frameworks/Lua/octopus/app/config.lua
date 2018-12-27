local config = {} -- extension configuration

config.location = {
	{name = "/plaintext", script = "PlaintextController.lua"},
	{name = "/json", script = "JsonSerializationController.lua"},
	{name = "/db", script = "SingleQueryController.lua"},
	{name = "/queries", script = "MultipleQueriesController.lua"},
	{name = "/fortunes", script = "FortunesController.lua"},
	{name = "/update", script = "UpdateController.lua"},
}

config.type = {
	"types.lua"
}

return config -- return extension configuration