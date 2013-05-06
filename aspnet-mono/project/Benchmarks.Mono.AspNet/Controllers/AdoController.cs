using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data;
using System.Data.Common;
using System.Web.Mvc;

using Benchmarks.Mono.AspNet.Models;

namespace Benchmarks.Mono.AspNet.Controllers
{
    public class AdoController : Controller
    {
        private DbConnection CreateConnection(string providerName)
        {
            ConnectionStringSettings connectionSettings = ConfigurationManager.ConnectionStrings[providerName];
            DbProviderFactory factory = DbProviderFactories.GetFactory(connectionSettings.ProviderName);
            DbConnection connection = factory.CreateConnection();
            connection.ConnectionString = connectionSettings.ConnectionString;
            return connection;
        }
        
        public ActionResult Index(string providerName, int? queries)
        {
            List<World> worlds = new List<World>(queries ?? 1);
            
            using (DbConnection connection = CreateConnection(providerName))
            {
                connection.Open();
                
                using (DbCommand command = connection.CreateCommand())
                {
                    command.CommandText = "SELECT * FROM World WHERE id = @ID";
                    
                    Random random = new Random();
                    
                    for (int i = 0; i < worlds.Capacity; i++)
                    {
                        int randomID = random.Next(0, 10000) + 1;
                        
                        DbParameter parameter = command.CreateParameter();
                        parameter.ParameterName = "@ID";
                        parameter.Value = randomID;
                        
                        command.Parameters.Clear();
                        command.Parameters.Add(parameter);
                        
                        using (DbDataReader reader = command.ExecuteReader(CommandBehavior.SingleRow))
                        {
                            if (reader.Read())
                            {
                                World world = new World();
                                world.id = reader.GetInt32(0);
                                world.randomNumber = reader.GetInt32(1);
                                
                                worlds.Add(world);
                            }
                        }
                    }
                }
            }
            
            return queries != null ? Json(worlds, JsonRequestBehavior.AllowGet)
                                   : Json(worlds[0], JsonRequestBehavior.AllowGet);
        }
        
        public ActionResult Fortunes(string providerName)
        {
            List<Fortune> fortunes = new List<Fortune>();
            
            using (DbConnection connection = CreateConnection(providerName))
            {
                connection.Open();
                
                using (DbCommand command = connection.CreateCommand())
                {
                    command.CommandText = "SELECT * FROM Fortune";
                    
                    using (DbDataReader reader = command.ExecuteReader(CommandBehavior.SequentialAccess))
                    {
                        while (reader.Read())
                        {
                            Fortune fortune = new Fortune
                            {
                                ID = reader.GetInt32(0),
                                Message = reader.GetString(1)
                            };
                            
                            fortunes.Add(fortune);
                        }
                    }
                }
            }
            
            fortunes.Add(new Fortune { ID = 0, Message = "Additional fortune added at request time." });
            fortunes.Sort();
            
            return View("Fortunes", fortunes);
        }
    }
}