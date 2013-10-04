using System;
using System.Collections.Generic;
using System.Configuration;
using System.Linq;
using System.Web.Mvc;

using MongoDB.Driver.Builders;

using Benchmarks.AspNet.Models;

namespace Benchmarks.AspNet.Controllers
{
    public class MongoDBController : Controller
    {
        Random random = new Random();
        
        public ActionResult Index(int? queries)
        {
            List<World> worlds = new List<World>(Math.Max(1, Math.Min(500, queries ?? 1)));

            Models.MongoDB db = new Models.MongoDB("MongoDB");

            for (int i = 0; i < worlds.Capacity; i++)
            {
                int randomID = random.Next(0, 10000) + 1;
                worlds.Add(db.Worlds.FindOne(Query<World>.EQ(w => w.id, randomID)));
            }

            return queries != null ? Json(worlds, JsonRequestBehavior.AllowGet)
                                   : Json(worlds[0], JsonRequestBehavior.AllowGet);
        }

        public ActionResult Fortunes()
        {
            Models.MongoDB db = new Models.MongoDB("MongoDB");

            List<Fortune> fortunes = db.Fortunes.FindAll().ToList();

            fortunes.Add(new Fortune { ID = 0, Message = "Additional fortune added at request time." });
            fortunes.Sort();

            return View("Fortunes", fortunes);
        }

        public ActionResult Update(int? queries)
        {
            Models.MongoDB db = new Models.MongoDB("MongoDB");

            List<World> worlds = new List<World>(Math.Max(1, Math.Min(500, queries ?? 1)));

            for (int i = 0; i < worlds.Capacity; i++)
            {
                int randomID = random.Next(0, 10000) + 1;
                int randomNumber = random.Next(0, 10000) + 1;

                World world = db.Worlds.FindOne(Query<World>.EQ(w => w.id, randomID));
                world.randomNumber = randomNumber;
                worlds.Add(world);

                db.Worlds.Save(world);
            }

            return Json(worlds, JsonRequestBehavior.AllowGet);
        }
    }
}