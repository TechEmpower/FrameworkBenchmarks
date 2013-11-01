using System;
using System.Collections.Generic;
using System.Data.Entity;
using Newtonsoft.Json;
using Owin;

namespace Benchmarks.Katana
{
    public class Startup
    {
        Random rand = new Random();
        private const string DbConn = "SQLServer";

        public void Configuration(IAppBuilder app)
        {
            app.Map("/json", builder => builder.Run(async ctx =>
            {
                ctx.Response.ContentType = "application/json";
                var response = await JsonConvert.SerializeObjectAsync(new {message = "Hello, World!"});
                await ctx.Response.WriteAsync(response);
            }));

            app.Map("/plaintext", builder => builder.Run(ctx =>
            {
                ctx.Response.ContentType = "text/plain";
                return ctx.Response.WriteAsync("Hello, World");
            }));

            app.Map("/db", builder => builder.Run(async ctx =>
            {
                var i = rand.Next(1, 10000);

                using (var db = new WorldContext(DbConn))
                {
                    var world = await db.World.AsNoTracking().FirstAsync(w => w.id == i);
                    var response = await JsonConvert.SerializeObjectAsync(world);
                    await ctx.Response.WriteAsync(response);
                }
            }));

            app.Map("/queries", builder => builder.Run(async ctx =>
            {
                var queries = 1;
                
                if(int.TryParse(ctx.Request.Query["queries"], out queries))
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
                    for (var i = 0; i < queries; i++)
                    {
                        var n = rand.Next(1, 10000);
                        worlds.Add(await db.World.AsNoTracking().FirstAsync(w => w.id == n));
                    }

                    var response = await JsonConvert.SerializeObjectAsync(worlds);
                    await ctx.Response.WriteAsync(response);
                }
            }));
        }
    }
}