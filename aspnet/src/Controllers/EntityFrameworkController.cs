using System;
using System.Collections.Generic;
using System.Web.Mvc;

using Benchmarks.AspNet.Models;

namespace Benchmarks.AspNet.Controllers
{
    public class EntityFrameworkController : Controller
    {
        Random random = new Random();

        public ActionResult Index(string providerName, int? queries)
        {
            List<World> worlds = new List<World>(Math.Max(1, Math.Min(500, queries ?? 1)));

            using (EntityFramework db = new EntityFramework(providerName))
            {
                for (int i = 0; i < worlds.Capacity; i++)
                {
                    int randomID = random.Next(0, 10000) + 1;
                    worlds.Add(db.Worlds.Find(randomID));
                }
            }

            return queries != null ? Json(worlds, JsonRequestBehavior.AllowGet)
                                   : Json(worlds[0], JsonRequestBehavior.AllowGet);
        }

        public ActionResult Fortunes(string providerName)
        {
            List<Fortune> fortunes = new List<Fortune>();

            using (EntityFramework db = new EntityFramework(providerName))
            {
                fortunes.AddRange(db.Fortunes);
            }

            fortunes.Add(new Fortune { ID = 0, Message = "Additional fortune added at request time." });
            fortunes.Sort();

            return View("Fortunes", fortunes);
        }

        public ActionResult Update(string providerName, int? queries)
        {
            List<World> worlds = new List<World>(Math.Max(1, Math.Min(500, queries ?? 1)));

            using (EntityFramework db = new EntityFramework(providerName))
            {
                for (int i = 0; i < worlds.Capacity; i++)
                {
                    int randomID = random.Next(0, 10000) + 1;
                    int randomNumber = random.Next(0, 10000) + 1;
                    
                    World world = db.Worlds.Find(randomID);
                    world.randomNumber = randomNumber;
                    worlds.Add(world);
                }

                // batch update
                db.SaveChanges();
            }

            return Json(worlds, JsonRequestBehavior.AllowGet);
        }
    }
}