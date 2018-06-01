using System;
using System.Collections.Generic;
using System.Data.Common;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Benchmarks.Configuration;
using Microsoft.Extensions.Options;
using Raven.Client;
using Raven.Client.Documents;
using Raven.Client.Documents.Commands;
using Raven.Client.Documents.Conventions;
using Raven.Client.Documents.Session;

namespace Benchmarks.Data
{
    public class RavenDb
    {        
        private readonly IRandom _random;
        private readonly IDocumentStore _worldStore;
        private readonly IDocumentStore _fortuneStore;
        private readonly string _connectionString;

        private readonly string[] _identifiers;

        public RavenDb(IRandom random, IOptions<AppSettings> appSettings)
        {
            _random = random;
            _connectionString = appSettings.Value.ConnectionString;

            _identifiers = new string[10001];
            for (var i = 0; i < 10001; i++)
                _identifiers[i] = $"{_random.Next(1, 10001)}";

            // We need to use different databases because we id's are integer and RavenDB does not have tables, it has collections
            // the semantics of the collections is entirely different from a table. The most similar thing is having an actual
            // database to isolate ids and guarantee similar semantics.
            var wstore = new DocumentStore
            {
                Urls = new[] { _connectionString },
                Database = "world",
            };

            wstore.Conventions.FindCollectionName = type =>
            {
                if (typeof(WorldRaven).IsAssignableFrom(type))
                    return "World";

                return DocumentConventions.DefaultGetCollectionName(type);
            };

            var fstore = new DocumentStore
            {
                Urls = new[] { _connectionString },
                Database = "fortunes",
            };

            fstore.Conventions.FindCollectionName = type =>
            {
                if (typeof(FortuneRaven).IsAssignableFrom(type))
                    return "Fortune";

                return DocumentConventions.DefaultGetCollectionName(type);
            };

            wstore.Initialize();
            fstore.Initialize();

            _worldStore = wstore;
            _fortuneStore = fstore;
        }
        
        public async ValueTask<WorldRaven> LoadSingleQueryRow()
        {
            using (var session = _worldStore.OpenAsyncSession())
            {
                return await session.LoadAsync<WorldRaven>(_identifiers[_random.Next(1, 10001)]);
            }
        }

        public async ValueTask<IEnumerable<WorldRaven>> LoadMultipleQueriesRows(int count)
        {
            using (var session = _worldStore.OpenAsyncSession())
            {
                var ids = new string[count];

                for (var i = 0; i < count; i++)
                    ids[i] = _identifiers[_random.Next(1, 10001)];

                var result = await session.LoadAsync<WorldRaven>(ids);
                return result.Values;
            }
        }

        public async ValueTask<IEnumerable<WorldRaven>> LoadMultipleUpdatesRows(int count)
        {         
            using (var session = _worldStore.OpenAsyncSession())
            {
                var ids = new string[count];

                for (var i = 0; i < count; i++)
                    ids[i] = _identifiers[_random.Next(1, 10001)];

                var result = await session.LoadAsync<WorldRaven>(ids);
                foreach (var pair in result)
                {
                    pair.Value.RandomNumber = _random.Next(1, 10001);
                }

                await session.SaveChangesAsync();

                return result.Values;
            }
        }

        public async ValueTask<List<FortuneRaven>> LoadFortunesRows()
        {
            using (var session = _fortuneStore.OpenAsyncSession())
            {
                var list = new List<FortuneRaven>();

                // TODO: We need to have an static index and stream it completely. 
                var query = session.Advanced
                                   .AsyncRawQuery<FortuneRaven>("from Fortune order by id()");

                var results = await session.Advanced.StreamAsync(query);
                while (await results.MoveNextAsync())
                {
                    list.Add(results.Current.Document);
                }

                return list;
            }
        }
    }
}
