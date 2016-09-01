using System;
using System.Collections.Generic;
using System.IO;
using System.ServiceModel;
using System.ServiceModel.Web;
using System.Text;
using FrameworkBench;
using Revenj.Api;
using Revenj.DomainPatterns;
using Revenj.Serialization;
using Revenj.Utility;

namespace Revenj.Bench
{
	[ServiceContract(Namespace = "https://github.com/ngs-doo/revenj")]
	public interface IRestService
	{
		[OperationContract]
		[WebGet(UriTemplate = "/plaintext")]
		Stream PlainText();

		[OperationContract]
		[WebGet(UriTemplate = "/json")]
		Stream JSON();

		[OperationContract]
		[WebGet(UriTemplate = "/db")]
		Stream SingleQuery();

		[OperationContract]
		[WebGet(UriTemplate = "/queries/{count}")]
		Stream MultipleQueries(string count);

		[OperationContract]
		[WebGet(UriTemplate = "/updates/{count}")]
		Stream Updates(string count);

		[OperationContract]
		[WebGet(UriTemplate = "/fortunes")]
		Stream Fortunes();
	}

	public class RestService : IRestService
	{
		private static readonly ChunkedMemoryStream HelloWorld = ChunkedMemoryStream.Static();
		private static readonly string[] IDs = new string[10001];
		[ThreadStatic]
		private static Context Context;
		private static Context GetContext(IServiceProvider services)
		{
			if (Context == null)
				Context = new Context(services);
			Context.Stream.Reset();
			return Context;
		}

		static RestService()
		{
			var hwText = Encoding.UTF8.GetBytes("Hello, World!");
			HelloWorld.Write(hwText, 0, hwText.Length);
			HelloWorld.Position = 0;
			for (int i = 0; i < IDs.Length; i++)
				IDs[i] = i.ToString();
		}

		private Random Random = new Random(0);
		private readonly IServiceProvider Services;

		public RestService(IServiceProvider services)
		{
			this.Services = services;
		}

		public Stream PlainText()
		{
			ThreadContext.Response.ContentType = "text/plain";
			return HelloWorld;
		}

		private Stream ReturnJSON(IJsonObject value, ChunkedMemoryStream cms)
		{
			value.Serialize(cms);
			ThreadContext.Response.ContentType = "application/json";
			return cms;
		}

		public Stream JSON()
		{
			var ctx = GetContext(Services);
			return ReturnJSON(new Message { message = "Hello, World!" }, ctx.Stream);
		}

		public Stream SingleQuery()
		{
			var id = Random.Next(10000) + 1;
			var ctx = GetContext(Services);
			var world = ctx.WorldRepository.Find(IDs[id]);
			return ReturnJSON(world, ctx.Stream);
		}

		/* bulk loading of worlds. use such pattern for production code */
		private void LoadWorldsFast(int repeat, Context ctx)
		{
			var reader = ctx.BulkReader;
			var lazyResult = ctx.LazyWorlds;
			var worlds = ctx.Worlds;
			reader.Reset(true);
			for (int i = 0; i < repeat; i++)
			{
				var id = Random.Next(10000) + 1;
				lazyResult[i] = reader.Find<World>(IDs[id]);
			}
			reader.Execute();
			for (int i = 0; i < repeat; i++)
				worlds[i] = lazyResult[i].Value;
		}

		/* multiple roundtrips loading of worlds. don't write such production code */
		private void LoadWorldsSlow(int repeat, Context ctx)
		{
			var worlds = ctx.Worlds;
			var repository = ctx.WorldRepository;
			for (int i = 0; i < repeat; i++)
			{
				var id = Random.Next(10000) + 1;
				worlds[i] = repository.Find(IDs[id]);
			}
		}

		public Stream MultipleQueries(string count)
		{
			int repeat;
			int.TryParse(count, out repeat);
			if (repeat < 1) repeat = 1;
			else if (repeat > 500) repeat = 500;
			var ctx = GetContext(Services);
			LoadWorldsSlow(repeat, ctx);
			var cms = ctx.Stream;
			ctx.Worlds.Serialize(cms, repeat);
			ThreadContext.Response.ContentType = "application/json";
			return cms;
		}

		private static readonly Comparison<World> ASC = (l, r) => l.id - r.id;

		public Stream Updates(string count)
		{
			int repeat;
			int.TryParse(count, out repeat);
			if (repeat < 1) repeat = 1;
			else if (repeat > 500) repeat = 500;
			var ctx = GetContext(Services);
			LoadWorldsSlow(repeat, ctx);
			var result = new World[repeat];
			Array.Copy(ctx.Worlds, result, repeat);
			for (int i = 0; i < result.Length; i++)
				result[i].randomNumber = Random.Next(10000) + 1;
			Array.Sort(result, ASC);
			ctx.WorldRepository.Update(result);
			var cms = ctx.Stream;
			result.Serialize(cms);
			ThreadContext.Response.ContentType = "application/json";
			return cms;
		}

		private static readonly Comparison<KeyValuePair<int, string>> Comparison = (l, r) => string.Compare(l.Value, r.Value, StringComparison.Ordinal);

		public Stream Fortunes()
		{
			var ctx = GetContext(Services);
			var fortunes = ctx.FortuneRepository.Search();
			var list = new List<KeyValuePair<int, string>>(fortunes.Length + 1);
			foreach (var f in fortunes)
				list.Add(new KeyValuePair<int, string>(f.id, f.message));
			list.Add(new KeyValuePair<int, string>(0, "Additional fortune added at request time."));
			list.Sort(Comparison);
			var cms = ctx.Stream;
			var writer = cms.GetWriter();
			var template = new Fortunes(list, writer);
			template.TransformText();
			writer.Flush();
			cms.Position = 0;
			ThreadContext.Response.ContentType = "text/html; charset=UTF-8";
			return cms;
		}
	}
}