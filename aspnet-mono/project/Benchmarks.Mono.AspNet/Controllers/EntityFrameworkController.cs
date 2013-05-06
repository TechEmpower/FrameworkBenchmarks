using System;
using System.Collections.Generic;
using System.Web.Mvc;

using Benchmarks.Mono.AspNet.Models;

namespace Benchmarks.Mono.AspNet.Controllers
{
    public class EntityFrameworkController : Controller
    {
        public ActionResult Index(string providerName, int? queries)
        {
            List<World> worlds = new List<World>(queries ?? 1);

            using (EntityFramework db = new EntityFramework(providerName))
            {
                Random random = new Random();
                
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
    }
}