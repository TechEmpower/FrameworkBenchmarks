using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data;
using System.Data.Common;
using System.Web.Mvc;
using System.Threading.Tasks;

using Benchmarks.AspNet.Models;

namespace Benchmarks.AspNet.Controllers
{
    public class AdoController : Controller
    {
        Random random = new Random();

        private DbConnection CreateConnection(string providerName)
        {
            ConnectionStringSettings connectionSettings = ConfigurationManager.ConnectionStrings[providerName];
            DbProviderFactory factory = DbProviderFactories.GetFactory(connectionSettings.ProviderName);
            DbConnection connection = factory.CreateConnection();
            connection.ConnectionString = connectionSettings.ConnectionString;
            return connection;
        }
        
        public async Task<ActionResult> Index(string providerName, int? queries)
        {
            List<World> worlds = new List<World>(Math.Max(1, Math.Min(500, queries ?? 1)));
            
            using (DbConnection connection = CreateConnection(providerName))
            {
                await connection.OpenAsync();
                
                using (DbCommand command = connection.CreateCommand())
                {
                    command.CommandText = "SELECT * FROM World WHERE id = @ID";

                    for (int i = 0; i < worlds.Capacity; i++)
                    {
                        int randomID = random.Next(0, 10000) + 1;
                        
                        DbParameter parameter = command.CreateParameter();
                        parameter.ParameterName = "@ID";
                        parameter.Value = randomID;
                        
                        command.Parameters.Clear();
                        command.Parameters.Add(parameter);
                        
                        // Don't use CommandBehavior.SingleRow because that will make the MySql provider
                        // send two extra commands to limit the result to one row.
                        using (DbDataReader reader = await command.ExecuteReaderAsync())
                        {
                            if (await reader.ReadAsync())
                            {
                                worlds.Add(new World
                                {
                                    id = reader.GetInt32(0),
                                    randomNumber = reader.GetInt32(1)
                                });
                            }
                        }
                    }
                }
            }
            
            return queries != null ? Json(worlds, JsonRequestBehavior.AllowGet)
                                   : Json(worlds[0], JsonRequestBehavior.AllowGet);
        }
        
        public async Task<ActionResult> Fortunes(string providerName)
        {
            List<Fortune> fortunes = new List<Fortune>();
            
            using (DbConnection connection = CreateConnection(providerName))
            {
                await connection.OpenAsync();
                
                using (DbCommand command = connection.CreateCommand())
                {
                    command.CommandText = "SELECT * FROM Fortune";
                    
                    using (DbDataReader reader = await command.ExecuteReaderAsync(CommandBehavior.SequentialAccess))
                    {
                        while (await reader.ReadAsync())
                        {
                            fortunes.Add(new Fortune
                            {
                                ID = reader.GetInt32(0),
                                Message = reader.GetString(1)
                            });
                        }
                    }
                }
            }
            
            fortunes.Add(new Fortune { ID = 0, Message = "Additional fortune added at request time." });
            fortunes.Sort();
            
            return View("Fortunes", fortunes);
        }

        public async Task<ActionResult> Update(string providerName, int? queries)
        {
            List<World> worlds = new List<World>(Math.Max(1, Math.Min(500, queries ?? 1)));

            using (DbConnection connection = CreateConnection(providerName))
            {
                await connection.OpenAsync();

                using (DbCommand selectCommand = connection.CreateCommand(),
                                 updateCommand = connection.CreateCommand())
                {
                    selectCommand.CommandText = "SELECT * FROM World WHERE id = @ID";
                    updateCommand.CommandText = "UPDATE World SET randomNumber = @Number WHERE id = @ID";

                    for (int i = 0; i < worlds.Capacity; i++)
                    {
                        int randomID = random.Next(0, 10000) + 1;
                        int randomNumber = random.Next(0, 10000) + 1;

                        DbParameter idParameter = selectCommand.CreateParameter();
                        idParameter.ParameterName = "@ID";
                        idParameter.Value = randomID;

                        selectCommand.Parameters.Clear();
                        selectCommand.Parameters.Add(idParameter);

                        World world = null;

                        // Don't use CommandBehavior.SingleRow because that will make the MySql provider
                        // send two extra commands to limit the result to one row.
                        using (DbDataReader reader = await selectCommand.ExecuteReaderAsync())
                        {
                            if (await reader.ReadAsync())
                            {
                                world = new World
                                {
                                    id = reader.GetInt32(0),
                                    randomNumber = reader.GetInt32(1)
                                };
                            }
                        }

                        if (world == null)
                            continue;
                        
                        DbParameter idUpdateParameter = updateCommand.CreateParameter();
                        idUpdateParameter.ParameterName = "@ID";
                        idUpdateParameter.Value = randomID;

                        DbParameter numberParameter = updateCommand.CreateParameter();
                        numberParameter.ParameterName = "@Number";
                        numberParameter.Value = randomNumber;

                        updateCommand.Parameters.Clear();
                        updateCommand.Parameters.Add(idUpdateParameter);
                        updateCommand.Parameters.Add(numberParameter);

                        await updateCommand.ExecuteNonQueryAsync();
                        
                        world.randomNumber = randomNumber;
                        worlds.Add(world);
                    }
                }
            }

            return Json(worlds, JsonRequestBehavior.AllowGet);
        }
    }
}