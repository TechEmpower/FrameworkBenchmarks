local net = require("@lute/net")
local json = require("./json")

return {
	serve = function()
		net.serve(function(req)
			if req.path == "/plaintext" then
				return {
					status = 200,
					headers = {
						["Content-Type"] = "text/plain",
						["Server"] = "Lute",
					},
					body = "Hello, world!",
				}
			elseif req.path == "/json" then
				return {
					status = 200,
					headers = {
						["Content-Type"] = "application/json",
						["Server"] = "Lute",
					},
					body = json.serialize({ message = "Hello, world!" }),
				}
			else
				return {
					status = 404,
					body = "Not Found",
				}
			end
		end)
	end,
}
