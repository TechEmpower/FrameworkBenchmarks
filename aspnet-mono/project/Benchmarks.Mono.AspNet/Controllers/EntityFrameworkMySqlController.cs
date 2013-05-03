using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using System.Web.Mvc;

using Benchmarks.Mono.AspNet.Models;

namespace Benchmarks.Mono.AspNet.Controllers
{
    public class EntityFrameworkMySqlController : Controller
    {
        public ActionResult Index(int? queries)
        {
            List<World> worlds = new List<World>(queries ?? 1);

            using (EntityFramework db = new EntityFramework())
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

        public ActionResult Fortunes()
        {
            List<Fortune> fortunes = new List<Fortune>();

            using (EntityFramework db = new EntityFramework())
            {
                fortunes.AddRange(db.Fortunes);
            }

            fortunes.Add(new Fortune { ID = 0, Message = "Additional fortune added at request time." });
            fortunes.Sort();

            return View(fortunes);
        }
    }
}