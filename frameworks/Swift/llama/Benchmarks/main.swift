
typealias JsonSerializer = System.Text.Json.JsonSerializer;
typealias JsonSerializerOptions = System.Text.Json.JsonSerializerOptions;

struct JSONTestResponse {
    let message = "Hello, World!"
}

enum Constants {
    static let plainTextResponse = try! System.Text.Encoding.UTF8.GetBytes("Hello, World!")
    static let jsonResponseLength = 27
    static let jsonSerializerOptions = try! JsonSerializerOptions()
    static let cancellationToken = System.Threading.CancellationToken.None
}

do 
{
    let f_mapRoutes = try System.Action_1<Microsoft.AspNetCore.Routing.IEndpointRouteBuilder>(
        {
            routes in

            try routes.MapGet("/plaintext")
            {
                context in
                let payloadLength = Constants.plainTextResponse.Length
                let response = context.Response
                response.StatusCode = 200
                response.ContentType = "text/plain"
                response.ContentLength = Int64(payloadLength)

                return try response.Body.WriteAsync(Constants.plainTextResponse, 0, payloadLength, Constants.cancellationToken);
            };
            /*
            try routes.MapGet("/json")
            {
                context in

                let response = context.Response
                response.StatusCode = 200
                response.ContentType = "application/json"
                response.ContentLength = Int64(Constants.jsonResponseLength)

                // Error: static method 'SerializeAsync' requires that 'JSONTestResponse' conform to 'LlamaValue'
                return try JsonSerializer.SerializeAsync(response.Body, JSONTestResponse(), Constants.jsonSerializerOptions, Constants.cancellationToken);
            };
            */
        }
        );

    try Microsoft.Extensions.Hosting.Host.CreateDefaultBuilder()
        .ConfigureWebHostDefaults
        {
            webHostBuilder in

            try webHostBuilder
                .Configure
                {
                    app in

                    try app
                        .UseRouting()
                        .UseEndpoints(f_mapRoutes)
                        ;
                }
                ;
        }
        .Build()
        .Run()
        ;
}
catch let e as System.Exception 
{
    try! System.Console.WriteLine(e.ToString());
}

