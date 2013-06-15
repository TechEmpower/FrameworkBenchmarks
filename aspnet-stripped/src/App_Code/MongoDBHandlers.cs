using System;
using System.Collections.Generic;
using System.Configuration;
using System.Linq;
using System.Web;
using System.Web.Script.Serialization;

using MongoDB.Driver.Builders;

using Benchmarks.AspNet.Models;

public class MongoDBDbHandler : IHttpHandler
{
    bool IHttpHandler.IsReusable
    {
        get { return true; }
    }

    void IHttpHandler.ProcessRequest(HttpContext context)
    {
        Random random = new Random();
        
        int queries = Common.GetQueries(context.Request);
        List<World> worlds = new List<World>(queries);

        Benchmarks.AspNet.Models.MongoDB db = new Benchmarks.AspNet.Models.MongoDB("MongoDB");

        for (int i = 0; i < worlds.Capacity; i++)
        {
            int randomID = random.Next(0, 10000) + 1;
            worlds.Add(db.Worlds.FindOne(Query<World>.EQ(w => w.id, randomID)));
        }

        HttpResponse response = context.Response;
        response.ContentType = "application/json";
        response.Write(new JavaScriptSerializer().Serialize(
            worlds.Count > 1 ? (Object)worlds : (Object)worlds[0]));
    }
}

public partial class MongoDBFortunesPage : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        Benchmarks.AspNet.Models.MongoDB db = new Benchmarks.AspNet.Models.MongoDB("MongoDB");

        List<Fortune> fortunes = db.Fortunes.FindAll().ToList();

        fortunes.Add(new Fortune { ID = 0, Message = "Additional fortune added at request time." });
        fortunes.Sort();

        Fortunes = fortunes;
    }

    public List<Fortune> Fortunes
    {
        get; set;
    }
}

public class MongoDBUpdatesHandler : IHttpHandler
{
    bool IHttpHandler.IsReusable
    {
        get { return true; }
    }

    void IHttpHandler.ProcessRequest(HttpContext context)
    {
        Random random = new Random();
        
        Benchmarks.AspNet.Models.MongoDB db = new Benchmarks.AspNet.Models.MongoDB("MongoDB");

        int queries = Common.GetQueries(context.Request);
        List<World> worlds = new List<World>(queries);

        for (int i = 0; i < worlds.Capacity; i++)
        {
            int randomID = random.Next(0, 10000) + 1;
            int randomNumber = random.Next(0, 10000) + 1;

            World world = db.Worlds.FindOne(Query<World>.EQ(w => w.id, randomID));
            world.randomNumber = randomNumber;
            worlds.Add(world);

            db.Worlds.Save(world);
        }

        HttpResponse response = context.Response;
        response.ContentType = "application/json";
        response.Write(new JavaScriptSerializer().Serialize(
            worlds.Count > 1 ? (Object)worlds : (Object)worlds[0]));
    }
}