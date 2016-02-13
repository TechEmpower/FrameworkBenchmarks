using System;
using System.IO;
using FrameworkBench;
using Revenj.DatabasePersistence;
using Revenj.DatabasePersistence.Postgres;
using Revenj.DomainPatterns;
using Revenj.Extensibility;
using Revenj.Utility;

namespace Revenj.Bench
{
	internal class Context
	{
		public readonly ChunkedMemoryStream Stream;
		public readonly TextWriter Writer;
		public readonly IPersistableRepository<World> Repository;
		public readonly IRepositoryBulkReader BulkReader;
		public readonly Lazy<World>[] LazyWorlds = new Lazy<World>[512];
		public readonly World[] Worlds = new World[512];

		public Context(IServiceProvider service)
		{
			Stream = ChunkedMemoryStream.Static();
			Writer = Stream.GetWriter();
			var dqm = service.Resolve<IDatabaseQueryManager>();
			var factory = service.Resolve<IObjectFactory>().CreateInnerFactory();
			factory.RegisterInterfaces(dqm.StartQuery(false));
			Repository = factory.Resolve<IPersistableRepository<World>>();
			BulkReader = factory.BulkRead(ChunkedMemoryStream.Static());
		}
	}
}
