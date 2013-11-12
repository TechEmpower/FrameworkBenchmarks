using System;
using System.Collections.Generic;
using System.Data.Entity;
using Benchmarks.Katana;
using Microsoft.Owin;
using Newtonsoft.Json;
using Owin;

[assembly:OwinStartup(typeof(Startup))]

namespace Benchmarks.Katana
{
    public class Startup
    {
        readonly Random _rand = new Random();
        private const string DbConn = "SQLServer";

        public void Configuration(IAppBuilder app)
        {
            app.Run(async ctx =>
            {
                string jsonResponse;

                switch (ctx.Request.Path.Value)
                {
                    case "/json":
                        ctx.Response.ContentType = "application/json";
                        jsonResponse = JsonConvert.SerializeObject(new {message = "Hello, World!"});
                        await ctx.Response.WriteAsync(jsonResponse);
                        break;

                    case "/plaintext":
                        ctx.Response.ContentType = "text/plain";
                        await ctx.Response.WriteAsync("Hello, World");
                        break;

                    case "/db":
                        var i = _rand.Next(1, 10000);

                        using (var db = new WorldContext(DbConn))
                        {
                            var world = await db.World.AsNoTracking().FirstAsync(w => w.id == i);
                            jsonResponse = JsonConvert.SerializeObject(world);
                            await ctx.Response.WriteAsync(jsonResponse);
                        }
                        break;

                    case "/queries":
                        int queries;

                        if (int.TryParse(ctx.Request.Query["queries"], out queries))
                        {
                            queries = queries < 1 ? 1 : queries;
                            queries = queries > 500 ? 500 : queries;
                        }
                        else
                        {
                            queries = 1;
                        }

                        var worlds = new List<World>(queries);

                        using (var db = new WorldContext(DbConn))
                        {
                            for (var j = 0; j < queries; j++)
                            {
                                var n = _rand.Next(1, 10000);
                                worlds.Add(await db.World.AsNoTracking().FirstAsync(w => w.id == n));
                            }

                            jsonResponse = JsonConvert.SerializeObject(worlds);
                            await ctx.Response.WriteAsync(jsonResponse);
                        }

                        break;
                }

            });
        }
    }
}