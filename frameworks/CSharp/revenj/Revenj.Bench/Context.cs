using System;
using FrameworkBench;
using Revenj.DatabasePersistence;
using Revenj.DomainPatterns;
using Revenj.Extensibility;

namespace Revenj.Bench
{
	internal class Context
	{
		public readonly IPersistableRepository<World> WorldRepository;
		public readonly IQueryableRepository<Fortune> FortuneRepository;
		public readonly Random Random = new Random(0);
		public readonly World[] Worlds = new World[512];
		private readonly IDatabaseQueryManager QueryManager;
		private readonly IDatabaseQuery Query;
		//private readonly IRepositoryBulkReader BulkReader;
		//private readonly Lazy<World>[] LazyWorlds = new Lazy<World>[512];

		public Context(IObjectFactory factory, IDatabaseQueryManager manager)
		{
			QueryManager = manager;
			var scope = factory.CreateScope(null);
			Query = manager.StartQuery(false);
			scope.RegisterInterfaces(Query);
			WorldRepository = scope.Resolve<IPersistableRepository<World>>();
			FortuneRepository = scope.Resolve<IQueryableRepository<Fortune>>();
			//BulkReader = scope.BulkRead(ChunkedMemoryStream.Static());
		}

		/* bulk loading of worlds. use such pattern for production code */
		/*public void LoadWorldsFast(int repeat, World[] worlds)
		{
			BulkReader.Reset(true);
			for (int i = 0; i < repeat; i++)
			{
				var id = Random.Next(10000) + 1;
				LazyWorlds[i] = BulkReader.Find<World>(id.ToString());
			}
			BulkReader.Execute();
			for (int i = 0; i < repeat; i++)
				worlds[i] = LazyWorlds[i].Value;
		}*/

		/* multiple roundtrips loading of worlds. don't write such production code */
		public void LoadWorldsSlow(int repeat, World[] worlds)
		{
			for (int i = 0; i < repeat; i++)
			{
				var id = Random.Next(10000) + 1;
				worlds[i] = WorldRepository.Find(id);
			}
		}

		~Context()
		{
			QueryManager.EndQuery(Query, true);
		}
	}
}
