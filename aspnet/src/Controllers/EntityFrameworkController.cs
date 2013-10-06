using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using System.Web.Mvc;

using Benchmarks.AspNet.Models;

namespace Benchmarks.AspNet.Controllers
{
    public class EntityFrameworkController : Controller
    {
        Random random = new Random();

        public async Task<ActionResult> Index(string providerName, int? queries)
        {
            List<World> worlds = new List<World>(Math.Max(1, Math.Min(500, queries ?? 1)));

            using (EntityFramework db = new EntityFramework(providerName))
            {
                for (int i = 0; i < worlds.Capacity; i++)
                {
                    int randomID = random.Next(0, 10000) + 1;
                    worlds.Add(await db.Worlds.FindAsync(CancellationToken.None, randomID));
                }
            }

            return queries != null ? Json(worlds, JsonRequestBehavior.AllowGet)
                                   : Json(worlds[0], JsonRequestBehavior.AllowGet);
        }

        public async Task<ActionResult> Fortunes(string providerName)
        {
            List<Fortune> fortunes = new List<Fortune>();

            using (EntityFramework db = new EntityFramework(providerName))
            {
                fortunes.AddRange(await db.Fortunes.ToListAsync());
            }

            fortunes.Add(new Fortune { ID = 0, Message = "Additional fortune added at request time." });
            fortunes.Sort();

            return View("Fortunes", fortunes);
        }

        public async Task<ActionResult> Update(string providerName, int? queries)
        {
            List<World> worlds = new List<World>(Math.Max(1, Math.Min(500, queries ?? 1)));

            using (EntityFramework db = new EntityFramework(providerName))
            {
                for (int i = 0; i < worlds.Capacity; i++)
                {
                    int randomID = random.Next(0, 10000) + 1;
                    int randomNumber = random.Next(0, 10000) + 1;
                    
                    World world = await db.Worlds.FindAsync(CancellationToken.None, randomID);
                    world.randomNumber = randomNumber;
                    worlds.Add(world);
                }

                // batch update
                await db.SaveChangesAsync();
            }

            return Json(worlds, JsonRequestBehavior.AllowGet);
        }
    }
}