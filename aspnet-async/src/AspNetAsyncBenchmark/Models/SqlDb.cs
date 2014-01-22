using System;
using System.Collections.Generic;
using System.Collections.Concurrent;
using System.Configuration;
using System.Data;
using System.Data.Common;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace AspNetAsyncBenchmark.Models
{
	public static class SqlDb
	{
		private static readonly Lazy<Dictionary<string, Tuple<DbProviderFactory, string>>> _databases = new Lazy<Dictionary<string, Tuple<DbProviderFactory, string>>>(() =>
			{
				var factories = DbProviderFactories.GetFactoryClasses().Rows.Cast<DataRow>().Select(r => (string) r["InvariantName"]).ToDictionary(p => p, p => DbProviderFactories.GetFactory(p));
				return ConfigurationManager.ConnectionStrings.Cast<ConnectionStringSettings>().Where(cs => factories.ContainsKey(cs.ProviderName)).ToDictionary(cs => cs.ProviderName, cs => Tuple.Create(factories[cs.ProviderName], cs.ConnectionString));
			}, LazyThreadSafetyMode.ExecutionAndPublication);

		private static async Task<T> ExecuteCommandAsync<T>(string providerName, bool prepare, string commandText, IEnumerable<string> parameterNames, Func<DbCommand, Task<T>> execute)
		{
			var db = _databases.Value[providerName];

			using (var cnn = db.Item1.CreateConnection())
			{
				cnn.ConnectionString = db.Item2;

				using (var command = cnn.CreateCommand())
				{
					command.CommandText = commandText;

					//HACK: ugly hack because dotConnect provider does not support @ parameter prefix : http://forums.devart.com/viewtopic.php?t=21497
					if (command is Devart.Data.PostgreSql.PgSqlCommand) 
						command.CommandText = command.CommandText.Replace('@', ':');

					if (parameterNames != null)
					{
						foreach (var name in parameterNames)
						{
							var dbParameter = command.CreateParameter();
							dbParameter.ParameterName = name;
							command.Parameters.Add(dbParameter);
						}
					}

					await cnn.OpenAsync();

					//HACK: quick hack to not call prepare for SQL Server because it requires parameter types
					// SQL Server caches the execution plan by default anyway
					if (prepare && !(command is System.Data.SqlClient.SqlCommand))
						command.Prepare();

					return await execute(command);
				}
			}
		}

		public static async Task<IEnumerable<World>> GetRandomWorlds(string providerName, int count)
		{
			var results = new ConcurrentBag<World>();
			var rnd = new Random();

			return await ExecuteCommandAsync(providerName, true, "SELECT randomNumber FROM World WHERE id = @p_id", new[] { "p_id" },
				async command =>
				{
					for (int i = 0; i < count; i++)
					{
						int id = rnd.Next(0, 10000) + 1;
						command.Parameters["p_id"].Value = id;

						var result = await command.ExecuteScalarAsync();
						results.Add(new World { id = id, randomNumber = (int) result });
					}

					return results;
				});
		}

		public static async Task<IEnumerable<Fortune>> GetFortunes(string providerName)
		{
			var results = new ConcurrentBag<Fortune>();

			return await ExecuteCommandAsync(providerName, true, "SELECT id, message FROM Fortune", null,
				async command =>
				{
					using (var reader = await command.ExecuteReaderAsync())
					{
						while (await reader.ReadAsync())
							results.Add(new Fortune { Id = reader.GetInt32(0), Message = reader.GetString(1) });
					}

					return results;
				});
		}

		public static async Task<IEnumerable<World>> UpdateRandomWorlds(string providerName, int count)
		{
			var results = new ConcurrentBag<World>();
			var rnd = new Random();

			var worlds = await GetRandomWorlds(providerName, count);
			foreach (var world in worlds)
				world.randomNumber = rnd.Next(0, 10000) + 1;

			string update = string.Join(";",
				worlds.Select(w => string.Format("UPDATE World SET randomNumber = {0} WHERE id = {1}", w.randomNumber, w.id)));

			return await ExecuteCommandAsync(providerName, false, update, null,
				command =>
				{
					command.ExecuteNonQueryAsync();
					return Task.FromResult(worlds);
				});
		}
	}

	class ThreadSafeRandom
	{
		private readonly Random _rnd = new Random();

		public int Next(int min, int max)
		{
			lock (_rnd)
			{
				return _rnd.Next(min, max);
			}
		}
	}
}