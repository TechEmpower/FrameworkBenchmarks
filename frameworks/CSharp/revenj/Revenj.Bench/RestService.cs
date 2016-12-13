using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Threading;
using FrameworkBench;
using Revenj.Api;
using Revenj.DatabasePersistence;
using Revenj.DomainPatterns;
using Revenj.Extensibility;
using Revenj.Http;
using Revenj.Serialization;
using Revenj.Utility;

namespace Revenj.Bench
{
	[Controller("bench")]
	public class RestService
	{
		private static readonly ChunkedMemoryStream HelloWorld = ChunkedMemoryStream.Static();

		static RestService()
		{
			var hwText = Encoding.UTF8.GetBytes("Hello, World!");
			HelloWorld.Write(hwText, 0, hwText.Length);
			HelloWorld.Position = 0;
		}

		private readonly ThreadLocal<Context> Context;

		public RestService(IObjectFactory factory, IDatabaseQueryManager queryManager)
		{
			this.Context = new ThreadLocal<Context>(() => new Context(factory, queryManager));
		}

		[Route(HTTP.GET, "/plaintext", false)]
		public Stream PlainText(IResponseContext response)
		{
			response.ContentType = "text/plain";
			return HelloWorld;
		}

		[Route(HTTP.GET, "/json", false)]
		public Message JSON()
		{
			return new Message { message = "Hello, World!" };
		}

		[Route(HTTP.GET, "/db")]
		public World SingleQuery()
		{
			var ctx = Context.Value;
			var id = ctx.Random.Next(10000) + 1;
			return ctx.WorldRepository.Find(id);
		}

		//while IList<World> would work, it would fall back to IEnumerable<IJsonObject> which would create garbage
		[Route(HTTP.GET, "/queries/{count}")]
		public IList<IJsonObject> MultipleQueries(string count, IResponseContext response)
		{
			int repeat;
			int.TryParse(count, out repeat);
			if (repeat < 1) repeat = 1;
			else if (repeat > 500) repeat = 500;
			var ctx = Context.Value;
			ctx.LoadWorldsSlow(repeat, ctx.Worlds);
			return new ArraySegment<IJsonObject>(ctx.Worlds, 0, repeat);
		}

		private static readonly Comparison<World> ASC = (l, r) => l.id - r.id;

		[Route(HTTP.GET, "/updates/{count}")]
		public World[] Updates(string count)
		{
			int repeat;
			int.TryParse(count, out repeat);
			if (repeat < 1) repeat = 1;
			else if (repeat > 500) repeat = 500;
			var ctx = Context.Value;
			var result = new World[repeat];
			ctx.LoadWorldsSlow(repeat, result);
			for (int i = 0; i < result.Length; i++)
				result[i].randomNumber = ctx.Random.Next(10000) + 1;
			Array.Sort(result, ASC);
			ctx.WorldRepository.Update(result);
			return result;
		}

		private static readonly Comparison<KeyValuePair<int, string>> Comparison = (l, r) => string.Compare(l.Value, r.Value, StringComparison.Ordinal);

		[Route(HTTP.GET, "/fortunes")]
		public Fortunes Fortunes()
		{
			var ctx = Context.Value;
			var fortunes = ctx.FortuneRepository.Search();
			var list = new List<KeyValuePair<int, string>>(fortunes.Length + 1);
			foreach (var f in fortunes)
				list.Add(new KeyValuePair<int, string>(f.id, f.message));
			list.Add(new KeyValuePair<int, string>(0, "Additional fortune added at request time."));
			list.Sort(Comparison);
			return new Fortunes(list);
		}
	}
}