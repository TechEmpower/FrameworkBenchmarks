using System;
using System.Collections.Generic;
using System.Configuration;
using System.Web.Mvc;

using MongoDB.Driver;
using MongoDB.Driver.Builders;

using Benchmarks.Mono.AspNet.Models;

namespace Benchmarks.Mono.AspNet.Controllers
{
    public class MongoDBController : Controller
    {
        private static string connectionString = ConfigurationManager.ConnectionStrings["MongoDB"].ConnectionString;

        public ActionResult Index(int? queries)
        {
            MongoClient client = new MongoClient(connectionString);
            MongoServer server = client.GetServer();
            MongoDatabase database = server.GetDatabase("hello_world");
            MongoCollection<World> collection = database.GetCollection<World>("world");
            
            List<World> worlds = new List<World>(queries ?? 1);
            
            Random random = new Random();

            for (int i = 0; i < worlds.Capacity; i++)
            {
                int randomID = random.Next(0, 10000) + 1;
                worlds.Add(collection.FindOne(Query<World>.EQ(e => e.id, randomID)));
            }

            return queries != null ? Json(worlds, JsonRequestBehavior.AllowGet)
                                   : Json(worlds[0], JsonRequestBehavior.AllowGet);
        }
    }
}