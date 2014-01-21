using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using System.Web;
using System.Web.Mvc;

using AspNetAsyncBenchmark.Models;
using JSON = Jil.JSON;

namespace AspNetAsyncBenchmark.Controllers
{
	public class DbController
		: Controller
	{
		static readonly Random _rnd = new Random();

		public async Task<ActionResult> SingleQuery(string providerName)
		{
			var world = (await SqlDb.GetRandomWorlds(providerName, 1)).FirstOrDefault();

			return Content(JSON.Serialize(world));
		}

		public async Task<ActionResult> MultipleQueries(string providerName, int? queries)
		{
			queries = Math.Max(1, Math.Min(queries ?? 1, 500));

			var worlds = await SqlDb.GetRandomWorlds(providerName, queries.Value);

			return Content(JSON.Serialize(worlds.ToArray()));
		}

		public async Task<ActionResult> Fortunes(string providerName)
		{
			var fortunes = await SqlDb.GetFortunes(providerName);
			fortunes = fortunes
				.Concat(new[] { new Fortune { Id = 0, Message = "Additional fortune added at request time." } })
				.OrderBy(f => f.Message, StringComparer.CurrentCulture);

			return View(fortunes);
		}

		public async Task<ActionResult> Updates(string providerName, int? queries)
		{
			queries = Math.Max(1, Math.Min(queries ?? 1, 500));

			var worlds = await SqlDb.UpdateRandomWorlds(providerName, queries.Value);

			return Content(JSON.Serialize(worlds.ToArray()));
		}
	}
}