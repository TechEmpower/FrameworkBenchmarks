using Pkg
Pkg.activate(@__DIR__)

using Genie
using Genie.Renderer
using Genie.Renderer.Json
using Genie.Responses
using Dates

function date_now_header()
   "Date" => Dates.format(Dates.now(), Dates.RFC1123Format) * " GMT"
end

const contentTypePlainText = "Content-Type" => "text/plain"

route("/plaintext") do
   respond("Hello, World!", 200, Genie.Renderer.HTTPHeaders(contentTypePlainText, date_now_header()))
end

struct JsonMessage 
   message::String
end

route("/json") do
   json(JsonMessage("Hello, World!"), headers = Genie.Renderer.HTTPHeaders(date_now_header()))
end

up(8080, "0.0.0.0", async = false, reuseaddr = true)