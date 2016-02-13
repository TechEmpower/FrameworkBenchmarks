using System;
using System.ComponentModel;
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
		[Description("Plain text response")]
		Stream PlainText();

		[OperationContract]
		[WebGet(UriTemplate = "/json")]
		[Description("JSON response")]
		Stream JSON();

		[OperationContract]
		[WebGet(UriTemplate = "/db")]
		[Description("Single database query")]
		Stream SingleQuery();

		[OperationContract]
		[WebGet(UriTemplate = "/queries/{count}")]
		[Description("Multiple database queries")]
		Stream MultipleQueries(string count);

		[OperationContract]
		[WebGet(UriTemplate = "/updates/{count}")]
		[Description("Database updates")]
		Stream Updates(string count);
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
			var world = ctx.Repository.Find(IDs[id]);
			return ReturnJSON(world, ctx.Stream);
		}

		private void LoadWorlds(int repeat, Context ctx)
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

		public Stream MultipleQueries(string count)
		{
			int repeat;
			int.TryParse(count, out repeat);
			if (repeat < 1) repeat = 1;
			else if (repeat > 500) repeat = 500;
			var ctx = GetContext(Services);
			LoadWorlds(repeat, ctx);
			var cms = ctx.Stream;
			ctx.Worlds.Serialize(cms, repeat);
			ThreadContext.Response.ContentType = "application/json";
			return cms;
		}

		public Stream Updates(string count)
		{
			int repeat;
			int.TryParse(count, out repeat);
			if (repeat < 1) repeat = 1;
			else if (repeat > 500) repeat = 500;
			var ctx = GetContext(Services);
			LoadWorlds(repeat, ctx);
			var result = new World[repeat];
			Array.Copy(ctx.Worlds, result, repeat);
			for (int i = 0; i < result.Length; i++)
				result[i].randomNumber = Random.Next(10000) + 1;
			ctx.Repository.Update(result);
			var cms = ctx.Stream;
			result.Serialize(cms);
			ThreadContext.Response.ContentType = "application/json";
			return cms;
		}
	}
}
