using System;
using System.Collections.Generic;
using System.Web.Mvc;

using Benchmarks.Mono.AspNet.Models;

namespace Benchmarks.Mono.AspNet.Controllers
{
    public class EntityFrameworkPostgreSqlController : Controller
    {
        static Random random = new Random();

        public ActionResult Index(int? queries)
        {
            List<World> worlds = new List<World>();

            using (PostgreSqlWorldContext db = new PostgreSqlWorldContext())
            {
                for (int i = 0; i < (queries ?? 1); i++)
                {
                    int randomID = random.Next(0, 10000) + 1;
                    worlds.Add(db.World.Find(randomID));
                }
            }

            return queries != null ? Json(worlds, JsonRequestBehavior.AllowGet)
                                   : Json(worlds[0], JsonRequestBehavior.AllowGet);
        }
    }
}